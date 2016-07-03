unit tsdsltypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type StrictName = record
  Value: String;
end;

type StrictComment = record
  Value: String;
  Delimeter: String;
end;

type StrictDeclarationComment = record
  Value: String;
  Delimeter: String;
end;

type StrictDefinitionComment = record
  Value: String;
  Delimeter: String;
end;

type StrictSplittedComment = record
  Declaration: StrictDeclarationComment;
  Definition: StrictDefinitionComment;
  Delimeter: String;
end;

type StrictContent = record
  Value: String;
end;

type NamedAttribute = record
  Name:    StrictName;
  Comment: StrictSplittedComment;
  Content: StrictContent;
  IsRef:   Boolean;
end;
type NamedAttributes = array of NamedAttribute;


type NamedNode = record
  Name:       StrictName;
  Comment:    StrictSplittedComment;
  Children:   array of NamedNode;
  Attributes: NamedAttributes;
  IsRef:      Boolean;
end;
type NamedNodes = array of NamedNode;


implementation

end.

