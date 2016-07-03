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
    procedure NamedTreeWithAttributes;
    procedure TreeWithNestedChildren;

    procedure TreeWithNodes;
    procedure TreeWithAttributes;
    procedure TreeWithNodesAndAttributes;
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

procedure TreeStructInfoTestCase.NamedTreeWithAttributes;
begin
  with TreeStructInfo(
    Name(''),
    Comment(''),
    Nodes([]),
    Attributes([
      Attribute(
        Name('#1 attribute'),
        Comment('#1 attribute comment'),
        Content('#1 attribute content')),
      Attribute(
        Name('#2 attribute'),
        Comment('#2 attribute comment'),
        Content('#2 attribute content'))
    ])) do
  try
    AssertEquals(AttributeExists('#1 attribute'), True);
    AssertEquals(AttributeExists('#2 attribute'), True);

    AssertEquals(ReadAttributeComment('#1 attribute', '', ctDeclaration), '#1 attribute comment');
    AssertEquals(ReadAttributeComment('#2 attribute', '', ctDeclaration), '#2 attribute comment');

    AssertEquals(ReadString('#1 attribute', ''), '#1 attribute content');
    AssertEquals(ReadString('#2 attribute', ''), '#2 attribute content');
  finally
   Free;
  end;
end;

procedure TreeStructInfoTestCase.TreeWithNestedChildren;
begin
  with TreeStructInfo(
    Name('problematic?'),
    Comment(''),
    Nodes([
      Node(
        Name('problematic'),
        Comment(''),
        Nodes([
          RefNode(
            Name('test'),
            DeclarationComment('#test declaration comment'),
            Attributes([
              Attribute(
                Name('test attribute')
              )
            ])
          )
        ]),
        Attributes([
          Attribute(
            Name('attr'),
            Comment('Example comment'),
            Content('Example content')
          )
        ])
      )
    ])
  )
  do try
    AssertEquals(TreeName, 'problematic?');

    AssertEquals(ChildNodeExists('problematic'),      True);
    AssertEquals(AttributeExists('problematic\attr'), True);

    AssertEquals(ReadAttributeComment('problematic\attr', '', ctDeclaration), 'Example comment');
    AssertEquals(ReadString('problematic\attr', ''),                          'Example content');

    AssertEquals(ChildNodeExists('problematic\test'),                         True);
    AssertEquals(ReadChildNodeComment('problematic\test', '', ctDeclaration), '#test declaration comment');
    AssertEquals(AttributeExists('problematic\test\test attribute'),          True);
  finally
    Free;
  end;
end;

procedure TreeStructInfoTestCase.TreeWithNodes;
begin
  with TreeStructInfo(
    Name('tree with nodes'),
    Nodes([
      Node(
        Name('#1')),
      RefNode(
        Name('#2'))
    ])
  ) do try
    AssertEquals(ChildNodeExists('#1'), True);
    AssertEquals(ChildNodeExists('#2'), True);
  finally
    Free;
  end;
end;

procedure TreeStructInfoTestCase.TreeWithAttributes;
begin
  with TreeStructInfo(
    Name('tree with nodes'),
    Attributes([
      Attribute(
        Name('#1')),
      RefAttribute(
        Name('#2'))
    ])
  ) do try
    AssertEquals(AttributeExists('#1'), True);
    AssertEquals(AttributeExists('#2'), True);
  finally
    Free;
  end;
end;

procedure TreeStructInfoTestCase.TreeWithNodesAndAttributes;
begin
  with TreeStructInfo(
    Name('tree with nodes'),
    Nodes([
      Node(
        Name('#1')),
      RefNode(
        Name('#2'))
    ]),
    Attributes([
      Attribute(
        Name('#1')),
      RefAttribute(
        Name('#2'))
    ])
  ) do try
    AssertEquals(ChildNodeExists('#1'), True);
    AssertEquals(ChildNodeExists('#2'), True);

    AssertEquals(AttributeExists('#1'), True);
    AssertEquals(AttributeExists('#2'), True);
  finally
    Free;
  end;
end;

initialization

  RegisterTest(TreeStructInfoTestCase);
end.

