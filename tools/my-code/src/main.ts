import * as vscode from "vscode";

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
      "my-code.peek-definition-aside",
      async (editor) => {
        const document = editor.document;
        const viewColumn = editor.viewColumn;
        await vscode.commands.executeCommand("editor.action.revealDefinitionAside");
        await vscode.window.showTextDocument(
          document,
          viewColumn,
        );
      },
    ),
  );

  context.subscriptions.push(
    vscode.window.onDidChangeTextEditorSelection(async (selection) => {
      if (selection.kind == vscode.TextEditorSelectionChangeKind.Keyboard) {
        await vscode.commands.executeCommand("workbench.action.closeSidebar");
      }
    }),
  );
}

type GoTarget = "change" | "conflict" | "error" | "reference" | "occurrence" | "edit";
var current_target: GoTarget = "error";
async function go(direction: "next" | "prev", target?: GoTarget) {
  if (target) {
    current_target = target;
  }

  const dispatch = {
    change: [
      "workbench.action.editor.nextChange",
      "workbench.action.editor.previousChange",
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
      "workbench.action.navigateForwardInEditLocations"
    ],
  };

  const command = dispatch[current_target][direction == "next" ? 0 : 1];
  await vscode.commands.executeCommand(command);
}
