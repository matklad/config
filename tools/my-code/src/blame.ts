import * as vscode from "vscode";
import { spawn } from "child_process";

export function blame_register(context: vscode.ExtensionContext) {
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
    const currentCommit = await runGit(cwd, ["rev-parse", "HEAD"]);
    await runGit(cwd, ["switch", "-d", commit]);
    lastCommit = currentCommit;
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
