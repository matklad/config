import OpenAI from "openai";
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
      "my-code.proofread",
      proofreadCommand,
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

type GoTarget = "change" | "conflict" | "error" | "reference" | "occurrence";
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
  };

  const command = dispatch[current_target][direction == "next" ? 0 : 1];
  await vscode.commands.executeCommand(command);
}

async function proofreadCommand(
  textEditor: vscode.TextEditor,
  _edit: vscode.TextEditorEdit,
) {
  const selection = textEditor.selection;
  const text = textEditor.document.getText(selection);
  const corrected = await proofread(text);
  await textEditor.edit((edit) => edit.replace(selection, corrected));
}

async function proofread(input: string): Promise<string> {
  const openai = new OpenAI();
  const messages = [
    {
      role: "system",
      content:
        "Correct all the language usage, spelling, grammar, and punctuation mistakes in one pass.",
    },
    {
      role: "user",
      content: input,
    },
  ];
  const tools = [
    {
      type: "function",
      function: {
        name: "edit",
        parameters: {
          type: "object",
          properties: {
            edits: {
              type: "array",
              description: "Make multiple specific edits to the text.",
              items: {
                type: "object",
                properties: {
                  before: {
                    type: "string",
                    description: "A snippet of text to replace.",
                  },
                  after: {
                    type: "string",
                    description: "The text to replace it with",
                  },
                },
                required: ["before", "after"],
              },
            },
          },
          required: ["edits"],
        },
      },
    },
  ];

  // @ts-ignore
  const response = await openai.chat.completions.create({
    model: "gpt-4",
    messages,
    tools,
    tool_choice: { type: "function", function: { name: "edit" } },
  });

  const { edits } = JSON.parse(
    // @ts-ignore
    response.choices[0].message.tool_calls[0].function.arguments,
  );
  let output = input;
  const escapeRegex = (string: string) =>
    string.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
  if (edits) {
    for (const r of edits) {
      let before = "?!?";
      let after = "?!?";
      for (const k of Object.keys(r)) {
        if (k.toLowerCase() == "before") before = r[k];
        if (k.toLowerCase() == "after") after = r[k];
      }
      console.log(
        "applying " + JSON.stringify(before) + " -> " +
          JSON.stringify(after),
      );
      output = output.replace(
        new RegExp(
          escapeRegex(before).replace(/\s+/g, String.raw`\s+`),
        ),
        after,
      );
    }
  }
  return output;
}
