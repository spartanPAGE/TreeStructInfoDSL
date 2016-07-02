unit TestTreeStructInfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, TSDSL, TSInfoTypes;

type TreeStructInfoTestCase = class(TTestCase)
  published
    procedure NamedTree;
    procedure NamedTreeWithComment;
    procedure NamedTreeWithCommentAndEmptyNodesArray;
    procedure NamedTreeWithFlatChildNodes;
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
    AssertEquals(TreeName,            'tree name');
    AssertEquals(ReadTreeComment(''), 'tree comment');
  finally
    Free;
  end;
end;

procedure TreeStructInfoTestCase.NamedTreeWithCommentAndEmptyNodesArray;
begin
  with TreeStructInfo(
       Name('tree name'),
       Comment('tree comment'),
       Nodes([])) do
  try
    AssertEquals(TreeName,            'tree name');
    AssertEquals(ReadTreeComment(''), 'tree comment');

    AssertEquals(GetChildNodesCount,  0);
  finally
    Free;
  end;
end;

procedure TreeStructInfoTestCase.NamedTreeWithFlatChildNodes;
begin
  with TreeStructInfo(
       Name('tree name'),
       Comment('tree comment'),
       Nodes([
         Node(
           Name('#1 node'),
           Comment('#1 node comment')),
         Node(
           Name('#2 node'),
           Comment('#2 node comment'))
       ])) do
  try
    AssertEquals(TreeName,            'tree name');
    AssertEquals(ReadTreeComment(''), 'tree comment');

    AssertEquals(GetChildNodesCount,  2);
    AssertEquals(ChildNodeExists('#1 node'), True);
    AssertEquals(ChildNodeExists('#2 node'), True);

    AssertEquals(ReadChildNodeComment('#1 node', '', ctDeclaration), '#1 node comment');
    AssertEquals(ReadChildNodeComment('#2 node', '', ctDeclaration), '#2 node comment');
  finally
    Free;
  end;
end;

initialization

  RegisterTest(TreeStructInfoTestCase);
end.

