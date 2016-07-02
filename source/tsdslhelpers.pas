unit TSDSLHelpers;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, TSInfoFiles, TSInfoConsts, TSInfoTypes, TSDSLTypes;


procedure AddNodesToTree(
  var Tree:    TTSInfoTree;
  const Path:  String;
  const Nodes: NamedNodes
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

procedure AddNodesToTree(
  var Tree:    TTSInfoTree;
  const Path:  String;
  const Nodes: NamedNodes
);
var NNode: NamedNode;
var GluedPath: String;
begin
  for NNode in Nodes do
  begin
    GluedPath := GluePath(Path, NNode.Name.Value);
    Tree.CreateChildNode(Path, False, NNode.Name.Value);
    Tree.WriteChildNodeComment(
      GluedPath,
      NNode.Comment.Value,
      NNode.Comment.Delimeter,
      ctDeclaration
    );
    AddAttributesToTree(Tree, GluedPath, NNode.Attributes);
  end;
end;

procedure AddAttributesToTree(
  var Tree:    TTSInfoTree;
  const Path:  String;
  const Attrs: NamedAttributes
);
var Attr: NamedAttribute;
var GluedPath: String;
begin
  for Attr in Attrs do
  begin
    GluedPath := GluePath(Path, Attr.Name.Value);
    Tree.CreateAttribute(Path, False, Attr.Name.Value);
    Tree.WriteAttributeComment(
      GluedPath,
      Attr.Comment.Value,
      Attr.Comment.Delimeter,
      ctDeclaration
    );
    Tree.WriteString(GluedPath, Attr.Content.Value);
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

