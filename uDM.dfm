object DM: TDM
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 257
  Width = 438
  object RESTClient1: TRESTClient
    Accept = 'application/json, text/plain; q=0.9, text/html;q=0.8,'
    AcceptCharset = 'UTF-8, *;q=0.8'
    Params = <>
    Left = 40
    Top = 32
  end
  object RESTRequest1: TRESTRequest
    Client = RESTClient1
    Params = <>
    Response = RESTResponse1
    SynchronizedEvents = False
    Left = 144
    Top = 32
  end
  object NetHTTPClient1: TNetHTTPClient
    UserAgent = 'Embarcadero URI Client/1.0'
    Left = 40
    Top = 96
  end
  object NetHTTPRequest1: TNetHTTPRequest
    MethodString = 'GET'
    Client = NetHTTPClient1
    Left = 140
    Top = 96
  end
  object RESTResponse1: TRESTResponse
    Left = 231
    Top = 32
  end
end
