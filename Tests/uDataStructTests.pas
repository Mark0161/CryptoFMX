unit uDataStructTests;

interface

uses
  DUnitX.TestFramework;

type

  [TestFixture]
  TestCryptoFMX = class
  private
    procedure Test1;
  public
  var
    fJsonStr: String;
    procedure CheckForTestJsonData;
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    // Sample Methods
    // Simple single Test
    [Test]
    procedure LoadJsonData();
    // Test with TestCase Attribute to supply parameters.
    [Test]
    procedure LoadInvalidJsonData();
    [Test]
    procedure AssignToCryptoList();
  end;

implementation

uses SysUtils, Classes, System.Generics.Collections, IOUtils, uDataStruct,
  uDataStructHelpers;

procedure TestCryptoFMX.AssignToCryptoList;
var
  CryptoSource, CryptoDest: TRCryptoList;
  CryptoStruct: TCryptoStruct;
  CryptoLength, I: Integer;
  CompareSourceStr, CompareDestStr, str1, str2: String;
begin
  Assert.IsTrue(CryptoSource.LoadJSON(fJsonStr),
    'JSON DATA LOADED SUCCESSFULLY');
  CryptoLength := CryptoSource.fCryptoList.Count;
  CryptoDest := CryptoSource;
  var
    CryptoList: TObjectList<TCryptoStruct> := CryptoSource.fCryptoList;

  Assert.IsTrue(CryptoList.GetHashCode = CryptoSource.fCryptoList.GetHashCode);

  str1 := CryptoSource.ToString;
  str2 := CryptoDest.ToString;

  Assert.IsTrue(str1 = str2);
  // The default GetHashCode returns the object's memory address,
  // which is unique for each object by definition.
  Assert.IsFalse(CryptoDest.fCryptoList.GetHashCode = CryptoSource.fCryptoList.
    GetHashCode);
  Assert.IsTrue(CryptoDest.fCryptoList.Count = CryptoSource.fCryptoList.Count);
  Assert.IsTrue(CryptoDest.fCryptoList[2].ID = CryptoSource.fCryptoList[2].ID);
  // Check that the two data structures are independent
  CryptoSource.fCryptoList.Clear;
  Assert.IsTrue(CryptoSource.fCryptoList.Count = 0);
  Assert.IsTrue(CryptoDest.fCryptoList.Count = CryptoLength);
  Assert.IsTrue(CryptoDest.ToString = str1);
end;

procedure TestCryptoFMX.CheckForTestJsonData;
begin

end;

procedure TestCryptoFMX.LoadInvalidJsonData;
var
  CryptoList: TRCryptoList;
begin
  Assert.IsFalse(CryptoList.LoadJSON(''),
    'Correctly return false when Loading invalid JSON');
end;

procedure TestCryptoFMX.LoadJsonData;
var
  CryptoList: TRCryptoList;
begin
  Assert.IsTrue(CryptoList.LoadJSON(fJsonStr), 'JSON DATA LOADED SUCCESSFULLY');
end;

procedure TestCryptoFMX.Setup;
begin
  TDUnitX.CurrentRunner.Log('TCryptoListTestObject Setting up...');
  var
    FStringList: TStringList := TStringList.Create();
  try
    FStringList.LoadFromFile('TestData\ListAllCrypto.JSON');
    fJsonStr := FStringList.Text;
  finally
    FStringList.Free;
  end;
end;

procedure TestCryptoFMX.TearDown;
begin
end;

procedure TestCryptoFMX.Test1;
begin
end;


initialization

TDUnitX.RegisterTestFixture(TestCryptoFMX);

end.
