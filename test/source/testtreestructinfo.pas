unit TestTreeStructInfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, TSDSL;

type TreeStructInfoTestCase = class(TTestCase)
  published
    procedure NamedTree;
    procedure NamedTreeWithComment;
    procedure NamedTreeWithCommentAndEmptyNodesArray;
  end;



implementation

procedure TreeStructInfoTestCase.NamedTree;
begin
  with TreeStructInfo(Name('tree name')) do
  try
    AssertEquals(TreeName, 'tree name');
  finally
    Free;
  end;
end;

procedure TreeStructInfoTestCase.NamedTreeWithComment;
begin
  with TreeStructInfo(
       Name('tree name'),
       Comment('tree comment')) do
  try
    AssertEquals(TreeName,    'tree name');
    AssertEquals(ReadTreeComment(''), 'tree comment');
  finally
    Free;
  end;
end;

procedure TreeStructInfoTestCase.NamedTreeWithCommentAndEmptyNodesArray;
begin
  //todo;
end;


initialization

  RegisterTest(TreeStructInfoTestCase);
end.

