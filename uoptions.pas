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
unit uoptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, DBCtrls,
  ExtCtrls, Buttons, LCLType, sqlite3conn, sqldb, db,DefaultTranslator;

type

  { TFOptions }

  TFOptions = class(TForm)
    BSave: TBitBtn;
    BCancel: TBitBtn;
    CBCloseMinimized: TCheckBox;
    CBSysTray: TCheckBox;
    CBProfileClick: TComboBox;
    CBSaveMouse: TComboBox;
    CBDeleteMouse: TComboBox;
    CBSingleClick: TComboBox;
    CBStartMinimized: TCheckBox;
    LSingleClick: TLabel;
    LProfileClick: TLabel;
    LSaveMouse: TLabel;
    LDeleteMouse: TLabel;
    Label5: TLabel;
    Panel1: TPanel;
    procedure BSaveClick(Sender: TObject);
    procedure BCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure UpdateComboBox(Sender: TObject);
  private
    function FKey(Sender: TObject): TStringList;
    function FKeyAvalible(Sender: TObject; VFkey:String): Boolean;
    function FKeyToString(VKey: Integer): String;
    function StringToFKey(VKey: String): Integer;

  public
    const
      database = 'data.db';
  end;

var
  FOptions: TFOptions;

implementation
{$R *.lfm}

{ TFOptions }

var
  DBConnection   : TSQLite3Connection;
  SQLTransaction : TSQLTransaction;
  SQLQuery       : TSQLQuery;

procedure TFOptions.BSaveClick(Sender: TObject);
begin
  With SQLQuery do
    begin
      SQL.Clear;
      SQL.Add('UPDATE OPTIONS ');
      SQL.Add('SET "StartMinimized" = '''+BoolToStr(CBStartMinimized.Checked)+''', ');
      SQL.Add('"CloseMinimized" = '''+BoolToStr(CBCloseMinimized.Checked)+''', ');
      SQL.Add('"SysTray" = '''+BoolToStr(CBSysTray.Checked)+''', ');
      SQL.Add('"SingleClick" = '''+inttostr(StringToFKey(CBSingleClick.Text))+''', ');
      SQL.Add('"ProfileClick" = '''+inttostr(StringToFKey(CBProfileClick.Text))+''', ');
      SQL.Add('"SaveMouse" = '''+inttostr(StringToFKey(CBSaveMouse.Text))+''', ');
      SQL.Add('"DeleteMouse" = '''+inttostr(StringToFKey(CBDeleteMouse.Text))+''' ');
      SQL.Add('WHERE  "ID" = ''1'';');
      ExecSQL;
    end;
  SQLTransaction.Commit;
  Close;
end;

procedure TFOptions.BCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TFOptions.FormCreate(Sender: TObject);
begin
  //Database Connections
  DBConnection   := TSQLite3Connection.Create(nil);
  SQLTransaction := TSQLTransaction.Create(nil);
  SQLQuery       := TSQLQuery.Create(nil);

  With  DBConnection do
    begin
      HostName:='';
      DatabaseName:=GetAppConfigDir(False)+database;
      UserName:='SYSDBA';
      Password:='apple';
      Charset := 'UTF8';
      Params.Add('PAGE_SIZE=16384');
      Transaction := SQLTransaction;
    end;
  SQLQuery.DataBase   := DBConnection;

  CBSingleClick.Style  := csDropDown;
  CBProfileClick.Style := csDropDown;
  CBSaveMouse.Style    := csDropDown;
  CBDeleteMouse.Style  := csDropDown;

  With SQLQuery do
    begin
      SQL.Clear;
      SQL.Add('SELECT ');
      SQL.Add('"StartMinimized", ');
      SQL.Add('"CloseMinimized", ');
      SQL.Add('"SysTray", ');
      SQL.Add('"SingleClick", ');
      SQL.Add('"ProfileClick", ');
      SQL.Add('"SaveMouse", ');
      SQL.Add('"DeleteMouse" ');
      SQL.Add('FROM OPTIONS; ');
      Open;

      CBStartMinimized.Checked := FieldByName('StartMinimized').AsBoolean;
      CBCloseMinimized.Checked := FieldByName('CloseMinimized').AsBoolean;
      CBSysTray.Checked        := FieldByName('SysTray').AsBoolean;

      CBSingleClick.Text  := FKeyToString(FieldByName('SingleClick').AsInteger);
      CBProfileClick.Text := FKeyToString(FieldByName('ProfileClick').AsInteger);
      CBSaveMouse.Text    := FKeyToString(FieldByName('SaveMouse').AsInteger);
      CBDeleteMouse.Text  := FKeyToString(FieldByName('DeleteMouse').AsInteger);

    end;

end;

procedure TFOptions.FormDestroy(Sender: TObject);
begin
  SQLQuery.free;
  SQLTransaction.free;
  DBConnection.free;
end;

procedure TFOptions.FormShow(Sender: TObject);
begin
  UpdateComboBox(CBDeleteMouse);
end;

procedure TFOptions.UpdateComboBox(Sender: TObject);
Var
  tSingleClick  : String;
  tProfileClick : String;
  tSaveMouse    : String;
  tDeleteMouse  : String;
begin

  tSingleClick  := CBSingleClick.Text;
  tProfileClick := CBProfileClick.Text;
  tSaveMouse    := CBSaveMouse.Text;
  tDeleteMouse  := CBDeleteMouse.Text;

  CBSingleClick.Items.Assign(FKey(CBSingleClick));
  CBProfileClick.Items.Assign(FKey(CBProfileClick));
  CBSaveMouse.Items.Assign(FKey(CBSaveMouse));
  CBDeleteMouse.Items.Assign(FKey(CBDeleteMouse));

  CBSingleClick.Style  := csDropDownList;
  CBProfileClick.Style := csDropDownList;
  CBSaveMouse.Style    := csDropDownList;
  CBDeleteMouse.Style  := csDropDownList;

  CBSingleClick.Text  := tSingleClick;
  CBProfileClick.Text := tProfileClick;
  CBSaveMouse.Text    := tSaveMouse;
  CBDeleteMouse.Text  := tDeleteMouse;

end;

function TFOptions.FKey(Sender: TObject): TStringList;
begin
  Result := TStringList.Create;
  if FKeyAvalible(Sender, 'F1') then Result.Add('F1');
  if FKeyAvalible(Sender, 'F2') then Result.Add('F2');
  if FKeyAvalible(Sender, 'F3') then Result.Add('F3');
  if FKeyAvalible(Sender, 'F4') then Result.Add('F4');
  if FKeyAvalible(Sender, 'F5') then Result.Add('F5');
  if FKeyAvalible(Sender, 'F6') then Result.Add('F6');
  if FKeyAvalible(Sender, 'F7') then Result.Add('F7');
  if FKeyAvalible(Sender, 'F8') then Result.Add('F8');
  if FKeyAvalible(Sender, 'F9') then Result.Add('F9');
  if FKeyAvalible(Sender, 'F10') then Result.Add('F10');
  if FKeyAvalible(Sender, 'F11') then Result.Add('F11');
  if FKeyAvalible(Sender, 'F12') then Result.Add('F12');
end;
function TFOptions.FKeyAvalible(Sender: TObject; VFkey:String): Boolean;
Var
  I : Integer;
begin
  Result := True;

  For I := 0 to ComponentCount - 1 do
    if Components[I].ClassType = TComboBox then
      if TComboBox(Components[I]).Text = VFkey then
        Result := False;


  IF TComboBox(Sender).Text = VFkey then
    Result := True;
end;
function TFOptions.FKeyToString(VKey: Integer): String;
begin
  case VKey of
    VK_F1:  Result := 'F1';
    VK_F2:  Result := 'F2';
    VK_F3:  Result := 'F3';
    VK_F4:  Result := 'F4';
    VK_F5:  Result := 'F5';
    VK_F6:  Result := 'F6';
    VK_F7:  Result := 'F7';
    VK_F8:  Result := 'F8';
    VK_F9:  Result := 'F9';
    VK_F10: Result := 'F10';
    VK_F11: Result := 'F11';
    VK_F12: Result := 'F12';
  end;
end;

function TFOptions.StringToFKey(VKey: String): Integer;
begin
  case VKey of
    'F1':  Result := VK_F1;
    'F2':  Result := VK_F2;
    'F3':  Result := VK_F3;
    'F4':  Result := VK_F4;
    'F5':  Result := VK_F5;
    'F6':  Result := VK_F6;
    'F7':  Result := VK_F7;
    'F8':  Result := VK_F8;
    'F9':  Result := VK_F9;
    'F10': Result := VK_F10;
    'F11': Result := VK_F11;
    'F12': Result := VK_F12;
  end;
end;

end.

