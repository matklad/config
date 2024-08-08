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
    hash = "sha256-ddaYXvbee66Keh5loQOs4xuOrtoQ06lHlBWlLiUp2zI=";
  };

  nativeBuildInputs = [
    python3.pkgs.setuptools
    python3.pkgs.wheel
  ];

  propagatedBuildInputs = with python3.pkgs; [
    httpx
    httpx-sse
    llm
  ];

  passthru.optional-dependencies = with python3.pkgs; {
    test = [
      pytest
      pytest-httpx
    ];
  };

  pythonImportsCheck = [ "llm_claude_3" ];
}
