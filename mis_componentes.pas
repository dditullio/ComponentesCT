{ This file was automatically created by Typhon. Do not edit!
  This source is only used to compile and install the package.
 }

unit mis_componentes;

interface

uses
  ControladorGrilla, SQLQueryGroup, zcontroladoredicion, zdatasetgroup, 
  zcontroladorgrilla, ControladorEdicion, DtDBEdit, DtDBLookupComboBox, 
  dtdbcoordedit, DtDBTimeEdit, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ControladorGrilla', @ControladorGrilla.Register);
  RegisterUnit('SQLQueryGroup', @SQLQueryGroup.Register);
  RegisterUnit('zcontroladoredicion', @zcontroladoredicion.Register);
  RegisterUnit('zdatasetgroup', @zdatasetgroup.Register);
  RegisterUnit('zcontroladorgrilla', @zcontroladorgrilla.Register);
  RegisterUnit('ControladorEdicion', @ControladorEdicion.Register);
  RegisterUnit('DtDBEdit', @DtDBEdit.Register);
  RegisterUnit('DtDBLookupComboBox', @DtDBLookupComboBox.Register);
  RegisterUnit('dtdbcoordedit', @dtdbcoordedit.Register);
  RegisterUnit('DtDBTimeEdit', @DtDBTimeEdit.Register);
end;

initialization
  RegisterPackage('mis_componentes', @Register);
end.
