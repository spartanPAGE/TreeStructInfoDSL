unit TSDSLHelpers;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, TSInfoFiles, TSInfoConsts, TSInfoTypes, TSDSLTypes;


procedure AddNodesToTree(var Tree: TTSInfoTree; const Nodes: NamedNodes);

implementation

procedure AddNodesToTree(var Tree: TTSInfoTree; const Nodes: NamedNodes);
var NNode: NamedNode;
begin
  for NNode in Nodes do
  begin
    Tree.CreateChildNode('', False, NNode.Name.Value);
    Tree.WriteChildNodeComment(NNode.Name.Value, NNode.Comment.Value, NNode.Comment.Delimeter, ctDeclaration);
  end;
end;


end.

