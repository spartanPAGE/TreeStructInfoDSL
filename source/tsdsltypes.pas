unit tsdsltypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type TStrictName = record
  Value: String;
end;

type TStrictComment = record
  Value: String;
  Delimeter: String;
end;

type TStrictDeclarationComment = record
  Value: String;
  Delimeter: String;
end;

type TStrictDefinitionComment = record
  Value: String;
  Delimeter: String;
end;

type TStrictSplittedComment = record
  Declaration: TStrictDeclarationComment;
  Definition: TStrictDefinitionComment;
  Delimeter: String;
end;

type TStrictContent = record
  Value: String;
end;

type TNamedAttribute = record
  Name:    TStrictName;
  Comment: TStrictSplittedComment;
  Content: TStrictContent;
  IsRef:   Boolean;
end;
type NamedAttributes = array of TNamedAttribute;


type TNamedNode = record
  Name:       TStrictName;
  Comment:    TStrictSplittedComment;
  Children:   array of TNamedNode;
  Attributes: NamedAttributes;
  IsRef:      Boolean;
end;
type TNamedNodes = array of TNamedNode;


implementation

end.

