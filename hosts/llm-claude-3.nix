{ lib
, python3
, fetchFromGitHub
}:

python3.pkgs.buildPythonApplication rec {
  pname = "llm-claude-3";
  version = "0.4";
  pyproject = true;

  src = fetchFromGitHub {
    owner = "simonw";
    repo = "llm-claude-3";
    rev = version;
    hash = "sha256-5qF5BK319PNzB4XsLdYvtyq/SxBDdHJ9IoKWEnvNRp4=";
  };

  nativeBuildInputs = [
    python3.pkgs.setuptools
    python3.pkgs.wheel
  ];

  propagatedBuildInputs = with python3.pkgs; [
    anthropic
  ];

  passthru.optional-dependencies = with python3.pkgs; {
    test = [
      pytest
    ];
  };

  dontCheckRuntimeDeps = true;
}
