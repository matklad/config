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
    vscode.window.onDidChangeTextEditorSelection(async (selection) => {
      if (selection.kind == vscode.TextEditorSelectionChangeKind.Keyboard) {
        await vscode.commands.executeCommand("workbench.action.closeSidebar");
      }
    }),
  );
}
