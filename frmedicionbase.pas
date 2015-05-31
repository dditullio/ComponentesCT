{ TODO : Sugerencia: Crear un componente no visual para pegar en el formulario y establecer algunas propiedades del form en este componente en lugar de tener que hacerlo por código.

La propiedades serían:
CampoDescripcion
IdPadre
EsSubform
Etc. }

unit frmedicionbase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, DB, FileUtil, LResources, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, Buttons, StdCtrls, ActnList, LCLType, DBCtrls,
  SQLQueryGroup, ControladorEdicion, DividerBevel;

const
  ED_AGREGAR = 0;
  ED_MODIFICAR = 1;
  ED_ELIMINAR = 2;
  ED_INDETERMINADO = 3;

type

  { TEdicionBase }

  TEdicionBase = class(TForm)
    bbCancelar: TBitBtn;
    bbAceptar: TBitBtn;
    cePrincipal: TControladorEdicion;
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
    sqPrincipal: TSQLQuery;
  private
  protected
  public
    { public declarations }
  end;

var
  EdicionBase: TEdicionBase;

implementation

initialization
  {$I frmedicionbase.lrs}

end.
