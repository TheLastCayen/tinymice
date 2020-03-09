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
unit udeleteprofile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  LCLType, lcltranslator;

type

  { TFDeleteProfile }

  TFDeleteProfile = class(TForm)
    BDelete: TBitBtn;
    BCancel: TBitBtn;
    EConfirm: TEdit;
    LConfirm1: TLabel;
    LConfirm2: TLabel;
    LConfirm3: TLabel;
    LConfirm4: TLabel;
    procedure BCancelClick(Sender: TObject);
    procedure BDeleteClick(Sender: TObject);
    procedure EConfirmChange(Sender: TObject);
    procedure EConfirmKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
  private

  public

  end;

var
  FDeleteProfile: TFDeleteProfile;

implementation

{$R *.lfm}

{ TFDeleteProfile }

procedure TFDeleteProfile.BDeleteClick(Sender: TObject);
begin
  if LowerCase(EConfirm.Text) = LowerCase(LConfirm2.Caption) then
    ModalResult:=mrOK
  else
    ShowMessage('Please type "delete" to confirm or click Cancel');
end;

procedure TFDeleteProfile.EConfirmChange(Sender: TObject);
begin
  if LowerCase(EConfirm.Text) =
  LowerCase(LConfirm2.Caption) then
    BDelete.Enabled := True
  else
    BDelete.Enabled := False;
end;

procedure TFDeleteProfile.EConfirmKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
    BDeleteClick(Sender);
end;

procedure TFDeleteProfile.BCancelClick(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

end.

