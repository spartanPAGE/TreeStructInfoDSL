program TreeStructInfoDSLTests;{$mode objfpc}{$H+}uses  Classes, consoletestrunner, TestTreeStructInfo, TestNamedAttribute;type  TMyTestRunner = class(TTestRunner) end;var  Application: TMyTestRunner;begin  Application := TMyTestRunner.Create(nil);  Application.Initialize;  Application.Title := 'FPCUnit Console test runner';  Application.Run;  Application.Free;end.