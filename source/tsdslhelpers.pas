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
    with Tree do
    begin
      CreateChildNode(Path, NNode.IsRef, NNode.Name.Value);
      WriteChildNodeComment(
        GluedPath,
        NNode.Comment.Declaration.Value,
        NNode.Comment.Delimeter,
        ctDeclaration
      );
      WriteChildNodeComment(
        GluedPath,
        NNode.Comment.Definition.Value,
        NNode.Comment.Delimeter,
        ctDefinition
      );
    end;
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

