unit mis_componentesReg;

{$mode objfpc}{$H+}

interface

procedure Register;

implementation

uses
  Classes, ControladorGrilla, LResources;

procedure Register;
begin
  RegisterComponents('Mis Componentes', [TControladorGrilla]);
end;

initialization
{$i mis_componentes.lrs}

end.

