unit frmzedicionbase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, DB, FileUtil, LResources, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, Buttons, StdCtrls, ActnList, LCLType, DBCtrls,
  SQLQueryGroup, ControladorEdicion, zcontroladoredicion, zdatasetgroup,
  DividerBevel, ZDataset;

const
  ED_AGREGAR = 0;
  ED_MODIFICAR = 1;
  ED_ELIMINAR = 2;
  ED_INDETERMINADO = 3;

type

  { TZEdicionBase }

  TZEdicionBase = class(TForm)
    bbCancelar: TBitBtn;
    bbAceptar: TBitBtn;
    dbtEncabezado: TDBText;
    DividerBevel1: TDividerBevel;
    dsPrincipal: TDatasource;
    laAccion: TLabel;
    paControles: TPanel;
    paEncabezadoBase: TPanel;
    paAccion: TPanel;
    paEncabezado: TPanel;
    paPrincipal: TPanel;
    qgPrincipal: TSQLQueryGroup;
    zcePrincipal: TZControladorEdicion;
    zdgPrincipal: TZDatasetGroup;
    zqPrincipal: TZQuery;
  private
    FSkipped: array of TWinControlClass;
  protected
  public
    { public declarations }
  end;

var
  ZEdicionBase: TZEdicionBase;

implementation

initialization
  {$I frmzedicionbase.lrs}

end.
