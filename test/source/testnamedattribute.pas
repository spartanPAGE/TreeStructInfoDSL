unit TestNamedAttribute;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, TSDSL, TSDSLTypes;

type

  NamedAttributeTestCase= class(TTestCase)
  private
    Attr: NamedAttribute;
  published
    procedure AttributeWithName;
    procedure AttributeWithNameAndComment;
    procedure AttributeWithNameCommentAndContent;
  end;

implementation

procedure NamedAttributeTestCase.AttributeWithName;
begin
  Attr := Attribute(
    Name('attr name')
  );

  AssertEquals(Attr.Name.Value, 'attr name');
end;

procedure NamedAttributeTestCase.AttributeWithNameAndComment;
begin
  Attr := Attribute(
    Name('attr name'),
    Comment('attr comment')
  );

  AssertEquals(Attr.Name.Value,                'attr name');
  AssertEquals(Attr.Comment.Declaration.Value, 'attr comment');
end;

procedure NamedAttributeTestCase.AttributeWithNameCommentAndContent;
begin
  Attr := Attribute(
    Name('attr name'),
    Comment('attr comment'),
    Content('attr content')
  );

  AssertEquals(Attr.Name.Value,                'attr name');
  AssertEquals(Attr.Comment.Declaration.Value, 'attr comment');
  AssertEquals(Attr.Content.Value,             'attr content');
end;


initialization

  RegisterTest(NamedAttributeTestCase);
end.

