import * as vscode from "vscode";
import { spawn } from "child_process";

export function activate_blame(context: vscode.ExtensionContext) {
    context.subscriptions.push(
        vscode.commands.registerCommand("inplace-blame.line", async () => {
            const editor = vscode.window.activeTextEditor;
            if (!editor) {
                vscode.window.showErrorMessage("No active editor");
                return;
            }

            const filePath = editor.document.uri.fsPath;
            const line = editor.selection.active.line + 1; // git is 1-based
            const cwd = vscode.workspace.getWorkspaceFolder(editor.document.uri)
                ?.uri
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
                vscode.window.showInformationMessage(
                    `Switched to commit ${commit}`,
                );
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

            const cwd = vscode.workspace.getWorkspaceFolder(editor.document.uri)
                ?.uri
                .fsPath;
            if (!cwd) {
                vscode.window.showErrorMessage("File is not in a workspace");
                return;
            }

            try {
                const currentCommit = await runGit(cwd, ["rev-parse", "HEAD"]);
                const parent = await runGit(cwd, [
                    "rev-parse",
                    `${currentCommit}^`,
                ]);

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

            const cwd = vscode.workspace.getWorkspaceFolder(editor.document.uri)
                ?.uri
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
}

let lastCommit: string | null = null;

async function switchCommit(cwd: string, commit: string) {
    const editor = vscode.window.activeTextEditor;
    const currentCommit = await runGit(cwd, ["rev-parse", "HEAD"]);

    // Capture file and line content before switching
    const originalUri = editor?.document.uri;
    const originalLineText = editor?.document.lineAt(editor.selection.active.line).text.trim();

    await runGit(cwd, ["switch", "-d", commit]);
    lastCommit = currentCommit;

    if (originalUri && originalLineText !== undefined) {
        await new Promise(resolve => setTimeout(resolve, 200));

        const doc = await vscode.workspace.openTextDocument(originalUri);
        const newEditor = await vscode.window.showTextDocument(doc, {
            preview: false,
            preserveFocus: false,
        });

        // Find the line with the same content
        let targetLine = 0;
        for (let i = 0; i < doc.lineCount; i++) {
            const text = doc.lineAt(i).text.trim();
            if (text === originalLineText) {
                targetLine = i;
                break;
            }
        }

        const pos = new vscode.Position(targetLine, 0);
        newEditor.selection = new vscode.Selection(pos, pos);
        newEditor.revealRange(
            new vscode.Range(pos, pos),
            vscode.TextEditorRevealType.InCenter,
        );
    }
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
                reject(
                    new Error(stderr.trim() || `git exited with code ${code}`),
                );
            }
        });
    });
}
