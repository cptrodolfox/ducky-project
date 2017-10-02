{ mkDerivation, acid-state, attoparsec, base, bytestring
, containers, mtl, safecopy, stdenv, text, time
}:
mkDerivation {
  pname = "the-project";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    acid-state attoparsec base bytestring containers mtl safecopy text
    time
  ];
  description = "A university project management system [small]";
  license = stdenv.lib.licenses.bsd3;
}
