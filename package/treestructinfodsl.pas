{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit TreeStructInfoDSL;

interface

uses
  TSInfoConsts, TSInfoFiles, TSInfoTypes, TSInfoUtils, tsdsl, tsdsltypes, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('TreeStructInfoDSL', @Register);
end.
