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
program ptinymice;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, uprofilename, udeleteprofile, uabout, uoptions, umain,
  lazmouseandkeyinput, Codebot.Input.Hotkeys, DefaultTranslator;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Title:='TinyMice';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TFMain, FMain);
  Application.Run;
end.

