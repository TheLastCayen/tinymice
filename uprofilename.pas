{
######################################################
#         Copyright 2020 Daniel Babin                #
######################################################

This file is part of TinyMice.

TinyMice is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

TinyMice is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with TinyMice.  If not, see <https://www.gnu.org/licenses/>.es/>.
}
unit uprofilename;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  LCLType, StdCtrls, Spin,DefaultTranslator;

type

  { TFProfileName }

  TFProfileName = class(TForm)
    BCancel: TBitBtn;
    BSave: TBitBtn;
    EProfileName: TEdit;
    LSP: TLabel;
    LProfileName: TLabel;
    SPClickInterval: TSpinEdit;
    procedure BCancelClick(Sender: TObject);
    procedure BSaveClick(Sender: TObject);
    procedure EProfileNameChange(Sender: TObject);
    procedure LENameKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private

  public

  end;

var
  FProfileName: TFProfileName;

implementation

{$R *.lfm}

{ TFProfileName }

procedure TFProfileName.BSaveClick(Sender: TObject);
begin
  ModalResult:=mrOK;
end;

procedure TFProfileName.EProfileNameChange(Sender: TObject);
begin
  if Trim(EProfileName.Text) <> '' then
    BSave.Enabled := True
  else
    BSave.Enabled := False;
end;

procedure TFProfileName.LENameKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
    BSaveClick(Sender);
end;


procedure TFProfileName.BCancelClick(Sender: TObject);
begin
  ModalResult:=mrAbort;
end;

end.

