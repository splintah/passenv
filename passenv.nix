{ mkDerivation, base, containers, data-default, exceptions
, megaparsec, mtl, process, stdenv, unix
}:
mkDerivation rec {
  pname = "passenv";
  version = "0.0.0";
  src = builtins.path {
    path = ./.;
    name = "${pname}-src";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers data-default exceptions megaparsec mtl process unix
  ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/splintah/passenv";
  description = "Fetches secrets from your password store and adds them to your environment";
  license = stdenv.lib.licenses.gpl3Plus;
}
