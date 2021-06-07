
```bash
# https://www.cosmiccode.blog/blog/vscode-for-ocaml/

opam switch create . ocaml-base-compiler.4.05.0
opam install merlin
opam user-setup install

opam install ocp-indent

# Code > Preferences > Settings
# "reason.path.env": "bash -c env",
#     "reason.path.bsb": "bash -ic bsb",
#     "reason.path.ocamlfind": "bash -ic ocamlfind",
#     "reason.path.ocamlmerlin": "bash -ic ocamlmerlin",
#     "reason.path.opam": "bash -ic opam",
#     "reason.path.rebuild": "bash -ic rebuild",
#     "reason.path.refmt": "bash -ic refmt",
#     "reason.path.refmterr": "bash -ic refmterr",
#     "reason.path.rtop": "bash -ic rtop",
#     "reason.diagnostics.tools": [
#         "merlin",
#         "bsb"
#     ],
#     "editor.formatOnSave": true,
#     "reason.codelens.enabled": true,

# Ctrl+P > Workspace Settings (JSON)
# {
#   "reason.path.ocamlmerlin": "bash -ic ./_opam/bin/ocamlmerlin",
#   "reason.path.ocamlfind": "bash -ic ./_opam/bin/ocamlfind",
#   "reason.path.ocpindent": "bash -ic ./_opam/bin/ocp-indent",
#   "reason.diagnostics.tools": [
#     "merlin"
#   ],
# }

```