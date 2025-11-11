import * as vscode from "vscode";
import { spawn } from "child_process";

export function activate(context: vscode.ExtensionContext) {
  context.subscriptions.push(
    vscode.commands.registerCommand("my-code.open-notes", async () => {
      const config = vscode.workspace.getConfiguration("my-code");
      const notesFile: string | undefined | null = config.get("notes-file");
      if (!notesFile) {
        await vscode.window.showErrorMessage(
          "`open-notes.file-path` config value not set",
        );
        return;
      }
      const uri = vscode.Uri.file(notesFile);
      const doc = await vscode.workspace.openTextDocument(uri);
      const editor = vscode.window.activeTextEditor;
      if (editor?.document == doc) {
        await vscode.commands.executeCommand(
          "workbench.action.closeActiveEditor",
        );
        return;
      }
      vscode.window.showTextDocument(doc);
    }),
  );

  context.subscriptions.push(
    vscode.commands.registerCommand("my-code.next", async (target) => {
      await go("next", target);
    }),
  );

  context.subscriptions.push(
    vscode.commands.registerCommand("my-code.prev", async (target) => {
      await go("prev", target);
    }),
  );

  context.subscriptions.push(
    vscode.commands.registerTextEditorCommand(
      "my-code.revealDefinitionAside",
      async (editor) => {
        console.log("reveal aside");
        const document = editor.document;
        const position = editor.selection.active;

        // Prepare definition locations
        const locations = await vscode.commands.executeCommand<
          (vscode.Location | vscode.LocationLink)[]
        >(
          "vscode.executeDefinitionProvider",
          document.uri,
          position,
        );

        if (!locations || locations.length === 0) {
          vscode.window.showInformationMessage("No definition found.");
          return;
        }

        console.log({ locations });

        const targetLocation: vscode.Location | vscode.LocationLink =
          locations[0]; // just use the first location

        const activeGroup = vscode.window.tabGroups.activeTabGroup;
        const allGroups = vscode.window.tabGroups.all;
        const isSecondGroupActive = activeGroup === allGroups[1];
        const uri = "uri" in targetLocation
          ? targetLocation.uri
          : targetLocation.targetUri;
        const range = "range" in targetLocation
          ? targetLocation.range
          : targetLocation.targetRange;

        const doc = await vscode.workspace.openTextDocument(uri);

        if (isSecondGroupActive && allGroups.length >= 2) {
          // Open in the first group (column 1) if second is active
          await vscode.window.showTextDocument(doc, {
            viewColumn: vscode.ViewColumn.One,
            selection: range,
            preserveFocus: false,
          });
        } else {
          // Otherwise, open to the side (default "Reveal Definition Aside" behavior)
          await vscode.commands.executeCommand(
            "editor.action.revealDefinitionAside",
          );
        }
      },
    ),
  );

  context.subscriptions.push(
    vscode.commands.registerCommand("inplace-blame.line", async () => {
      const editor = vscode.window.activeTextEditor;
      if (!editor) {
        vscode.window.showErrorMessage("No active editor");
        return;
      }

      const filePath = editor.document.uri.fsPath;
      const line = editor.selection.active.line + 1; // git is 1-based
      const cwd = vscode.workspace.getWorkspaceFolder(editor.document.uri)?.uri
        .fsPath;
      if (!cwd) {
        vscode.window.showErrorMessage("File is not in a workspace");
        return;
      }

      try {
        const blameOutput = await runGit(cwd, [
          "blame",
          `-L${line},${line}`,
          "--porcelain",
          filePath,
        ]);
        const commit = blameOutput.split("\n")[0].split(" ")[0];

        await switchCommit(cwd, commit);
        vscode.window.showInformationMessage(`Switched to commit ${commit}`);
      } catch (err: any) {
        vscode.window.showErrorMessage(`Error: ${err.message}`);
      }
    }),
  );

  context.subscriptions.push(
    vscode.commands.registerCommand("inplace-blame.parent", async () => {
      const editor = vscode.window.activeTextEditor;
      if (!editor) {
        vscode.window.showErrorMessage("No active editor");
        return;
      }

      const cwd = vscode.workspace.getWorkspaceFolder(editor.document.uri)?.uri
        .fsPath;
      if (!cwd) {
        vscode.window.showErrorMessage("File is not in a workspace");
        return;
      }

      try {
        const currentCommit = await runGit(cwd, ["rev-parse", "HEAD"]);
        const parent = await runGit(cwd, ["rev-parse", `${currentCommit}^`]);

        await switchCommit(cwd, parent);
        vscode.window.showInformationMessage(
          `Switched to parent commit ${parent}`,
        );
      } catch (err: any) {
        vscode.window.showErrorMessage(`Error: ${err.message}`);
      }
    }),
  );

  context.subscriptions.push(
    vscode.commands.registerCommand("inplace-blame.undo", async () => {
      const editor = vscode.window.activeTextEditor;
      if (!editor) {
        vscode.window.showErrorMessage("No active editor");
        return;
      }

      const cwd = vscode.workspace.getWorkspaceFolder(editor.document.uri)?.uri
        .fsPath;
      if (!cwd) {
        vscode.window.showErrorMessage("File is not in a workspace");
        return;
      }

      if (!lastCommit) {
        vscode.window.showWarningMessage("No undo commit available");
        return;
      }

      try {
        const commitToRestore = lastCommit;
        lastCommit = null;
        await runGit(cwd, ["switch", "-d", commitToRestore]);
        vscode.window.showInformationMessage(
          `Restored to commit ${commitToRestore}`,
        );
      } catch (err: any) {
        vscode.window.showErrorMessage(`Error: ${err.message}`);
      }
    }),
  );

  context.subscriptions.push(
    vscode.window.onDidChangeTextEditorSelection(async (selection) => {
      if (selection.kind == vscode.TextEditorSelectionChangeKind.Keyboard) {
        await vscode.commands.executeCommand("workbench.action.closeSidebar");
      }
    }),
  );

  context.subscriptions.push(vscode.commands.registerCommand(
    "my-code.reveal-in-magit",
    async () => {
      const editor = vscode.window.activeTextEditor;
      if (!editor) {
        vscode.window.showErrorMessage("No active editor.");
        return;
      }

      const currentFile = editor.document.uri.fsPath;
      const currentLine = editor.selection.active.line + 1; // 1-based

      // Find the Magit diff/status editor (right pane)
      const magitEditor = vscode.window.visibleTextEditors.find((e) =>
        e.document.uri.path.includes("status.magit") ||
        e.document.fileName.endsWith("status.magit")
      );

      if (!magitEditor) {
        return; // No Magit diff/status buffer found.
      }

      const diffLines = magitEditor.document.getText().split("\n");
      const fileName = currentFile.split("/").pop()!;

      let inCurrentFile = false;
      let hunkStart = -1;
      let hunkEnd = -1;

      for (let i = 0; i < diffLines.length; i++) {
        const line = diffLines[i];

        // Detect start of a file diff section
        const modifiedMatch = line.match(
          /^(modified|deleted|new file)\s+(.+)$/,
        );
        if (modifiedMatch) {
          const path = modifiedMatch[2].trim();
          inCurrentFile = path.endsWith(fileName);
          continue;
        }

        // Detect hunk headers for the current file
        if (inCurrentFile && line.startsWith("@@")) {
          const match = line.match(/@@ -\d+(?:,\d+)? \+(\d+)(?:,(\d+))? @@/);
          if (match) {
            const startNew = parseInt(match[1]);
            const lenNew = match[2] ? parseInt(match[2]) : 1;
            const endNew = startNew + lenNew - 1;

            if (currentLine >= startNew && currentLine <= endNew) {
              hunkStart = i;

              // find hunk end (next @@, next file header, or EOF)
              for (let j = i + 1; j < diffLines.length; j++) {
                const l = diffLines[j];
                if (
                  l.startsWith("@@") ||
                  l.match(/^(modified|deleted|new file)\s+/)
                ) {
                  hunkEnd = j - 1;
                  break;
                }
              }
              if (hunkEnd === -1) hunkEnd = diffLines.length - 1;
              break;
            }
          }
        }
      }

      if (hunkStart === -1) {
        return; // "Could not find matching diff hunk in Magit buffer.
      }

      const startPos = new vscode.Position(hunkStart, 0);
      const endPos = new vscode.Position(
        hunkEnd,
        diffLines[hunkEnd]?.length || 0,
      );
      const hunkRange = new vscode.Range(startPos, endPos);

      // Reveal and select the whole hunk
      magitEditor.revealRange(hunkRange, vscode.TextEditorRevealType.InCenter);
      magitEditor.selection = new vscode.Selection(startPos, endPos);
    },
  ));
}

function runGit(cwd: string, args: string[]): Promise<string> {
  return new Promise((resolve, reject) => {
    const proc = spawn("git", args, { cwd });
    let stdout = "";
    let stderr = "";

    proc.stdout.on("data", (data) => {
      stdout += data.toString();
    });

    proc.stderr.on("data", (data) => {
      stderr += data.toString();
    });

    proc.on("close", (code) => {
      if (code === 0) {
        resolve(stdout.trim());
      } else {
        reject(new Error(stderr.trim() || `git exited with code ${code}`));
      }
    });
  });
}

let lastCommit: string | null = null;

async function switchCommit(cwd: string, commit: string) {
  const currentCommit = await runGit(cwd, ["rev-parse", "HEAD"]);
  await runGit(cwd, ["switch", "-d", commit]);
  lastCommit = currentCommit;
}

type GoTarget =
  | "change"
  | "conflict"
  | "error"
  | "reference"
  | "occurrence"
  | "edit";
var current_target: GoTarget = "error";
async function go(direction: "next" | "prev", target?: GoTarget) {
  if (target) {
    current_target = target;
  }

  const dispatch = {
    change: [
      ["workbench.action.editor.nextChange", "my-code.reveal-in-magit"],
      ["workbench.action.editor.previousChange", "my-code.reveal-in-magit"],
    ],
    conflict: ["merge-conflict.next", "merge-conflict.previous"],
    error: [
      "editor.action.marker.nextInFiles",
      "editor.action.marker.prevInFiles",
    ],
    reference: [
      "references-view.next",
      "references-view.prev",
    ],
    occurrence: [
      "editor.action.nextMatchFindAction",
      "editor.action.previousMatchFindAction",
    ],
    edit: [
      "workbench.action.navigateBackInEditLocations",
      "workbench.action.navigateForwardInEditLocations",
    ],
  };

  const command = dispatch[current_target][direction == "next" ? 0 : 1];
  if (typeof command === "string") {
    await vscode.commands.executeCommand(command);
  } else {
    for (const subcommand of command) {
      await vscode.commands.executeCommand(subcommand);
    }
  }
}
