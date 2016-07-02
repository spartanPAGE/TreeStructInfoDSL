unit TestNamedNode;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, TSDSL, TSDSLTypes;

type

  NamedNodeTestCase= class(TTestCase)
  private
    NNode: NamedNode;
  published
    procedure NamedNode;
    procedure NamedNodeWithComment;
    procedure NamedNodeWithCommentAndFlatChildren;
    procedure NamedNodeWithCommentAndFlatChildrenAndAttributes;
    procedure NestedNodes;
  end;

implementation

procedure NamedNodeTestCase.NamedNode;
begin
  NNode := Node(
    Name('node name')
  );

  AssertEquals(NNode.Name.Value, 'node name');
end;

procedure NamedNodeTestCase.NamedNodeWithComment;
begin
  NNode := Node(
    Name('node name'),
    Comment('node comment')
  );

  AssertEquals(NNode.Name.Value,    'node name');
  AssertEquals(NNode.Comment.Value, 'node comment');
end;

procedure NamedNodeTestCase.NamedNodeWithCommentAndFlatChildren;
begin
  NNode := Node(
    Name('node name'),
    Comment('node comment'),
    Nodes([
      Node(Name('#1 child')),
      Node(Name('#2 child'))
    ])
  );

  AssertEquals(NNode.Name.Value,    'node name');
  AssertEquals(NNode.Comment.Value, 'node comment');

  AssertEquals(NNode.Children[0].Name.Value, '#1 child');
  AssertEquals(NNode.Children[1].Name.Value, '#2 child');
end;

procedure NamedNodeTestCase.NamedNodeWithCommentAndFlatChildrenAndAttributes;
begin
  NNode := Node(
    Name('node name'),
    Comment('node comment'),
    Nodes([
      Node(Name('#1 child')),
      Node(Name('#2 child'))
    ]),
    Attributes([
      Attribute(
        Name('#1 attr name'),
        Comment('#1 attr comment'),
        Content('#1 attr content')),
      Attribute(
        Name('#2 attr name'),
        Comment('#2 attr comment'),
        Content('#2 attr content'))
    ])
  );

  AssertEquals(NNode.Name.Value, 'node name');
  AssertEquals(NNode.Comment.Value, 'node comment');

  AssertEquals(NNode.Children[0].Name.Value, '#1 child');
  AssertEquals(NNode.Children[1].Name.Value, '#2 child');

  AssertEquals(NNode.Attributes[0].Name.Value,    '#1 attr name');
  AssertEquals(NNode.Attributes[0].Comment.Value, '#1 attr comment');
  AssertEquals(NNode.Attributes[0].Content.Value, '#1 attr content');

  AssertEquals(NNode.Attributes[1].Name.Value,    '#2 attr name');
  AssertEquals(NNode.Attributes[1].Comment.Value, '#2 attr comment');
  AssertEquals(NNode.Attributes[1].Content.Value, '#2 attr content');
end;

procedure NamedNodeTestCase.NestedNodes;
begin
  NNode := Node(
    Name('regular'),
    Comment('nothing interesting'),
    Nodes([
      Node(
        Name('problematic'),
        Comment('attrs problem'),
        Nodes([]),
        Attributes([
          //Attribute(Name('thats enough'))
        ])
      )
    ])
  );
end;



initialization

  RegisterTest(NamedNodeTestCase);
end.

