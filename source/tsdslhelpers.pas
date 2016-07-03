unit TSDSLHelpers;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, TSInfoFiles, TSInfoConsts, TSInfoTypes, TSDSLTypes;


procedure AddNodesToTree(
  var Tree:    TTSInfoTree;
  const Path:  String;
  const Nodes: TNamedNodes
);

procedure AddAttributesToTree(
  var Tree:    TTSInfoTree;
  const Path:  String;
  const Attrs: NamedAttributes
);

function GluePath(
  const Lhs:       String;
  const Rhs:       String;
  const Delimeter: String = '\'
): String;

implementation

procedure ApplyNodeComments(
  var Tree:   TTSInfoTree;
  const Path: String;
  const Node: TNamedNode
);
begin
  with Tree do
  begin
    WriteChildNodeComment(
      Path,
      Node.Comment.Declaration.Value,
      Node.Comment.Delimeter,
      ctDeclaration
    );
    WriteChildNodeComment(
      Path,
      Node.Comment.Definition.Value,
      Node.Comment.Delimeter,
      ctDefinition
    );
  end;
end;

procedure AddNodesToTree(
  var Tree:    TTSInfoTree;
  const Path:  String;
  const Nodes: TNamedNodes
);
var Node: TNamedNode;
var GluedPath: String;
begin
  for Node in Nodes do
  begin
    GluedPath := GluePath(Path, Node.Name.Value);

    Tree.CreateChildNode(Path, Node.IsRef, Node.Name.Value);

    ApplyNodeComments(Tree, GluedPath, Node);
    AddNodesToTree(Tree, GluedPath, Node.Children);
    AddAttributesToTree(Tree, GluedPath, Node.Attributes);
  end;
end;

procedure AddAttributesToTree(
  var Tree:    TTSInfoTree;
  const Path:  String;
  const Attrs: NamedAttributes
);
var Attr: TNamedAttribute;
var GluedPath: String;
begin
  for Attr in Attrs do
  begin
    GluedPath := GluePath(Path, Attr.Name.Value);
    with Tree do
    begin
      CreateAttribute(Path, Attr.IsRef, Attr.Name.Value);
      WriteAttributeComment(
        GluedPath,
        Attr.Comment.Declaration.Value,
        Attr.Comment.Delimeter,
        ctDeclaration
      );
      WriteAttributeComment(
        GluedPath,
        Attr.Comment.Definition.Value,
        Attr.Comment.Delimeter,
        ctDefinition
      );
      WriteString(GluedPath, Attr.Content.Value);
    end;
  end;
end;

function GluePath(
  const Lhs:       String;
  const Rhs:       String;
  const Delimeter: String
): String;
begin
  if Rhs = '' then
    GluePath := Lhs
  else if Lhs = '' then
    GluePath := Rhs
  else
    GluePath := Lhs + Delimeter + Rhs;
end;

end.

