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
begin
  for NNode in Nodes do
  begin
    Tree.CreateChildNode(Path, False, NNode.Name.Value);
    Tree.WriteChildNodeComment(
      GluePath(Path, NNode.Name.Value),
      NNode.Comment.Value,
      NNode.Comment.Delimeter,
      ctDeclaration
    );
  end;
end;

procedure AddAttributesToTree(
  var Tree:    TTSInfoTree;
  const Path:  String;
  const Attrs: NamedAttributes
);
begin
  //todo;
end;

function GluePath(
  const Lhs:       String;
  const Rhs:       String;
  const Delimeter: String
): String;
begin
  if Rhs = '' then
    GluePath := Lhs
  else
    GluePath := Lhs + Delimeter + Rhs;
end;

end.

