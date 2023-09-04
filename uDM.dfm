object DM: TDM
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 327
  Width = 633
  PixelsPerInch = 144
  object RESTClient1: TRESTClient
    Accept = 'application/json, text/plain; q=0.9, text/html;q=0.8,'
    AcceptCharset = 'UTF-8, *;q=0.8'
    Params = <>
    SynchronizedEvents = False
    Left = 36
    Top = 24
  end
  object RESTRequest1: TRESTRequest
    AssignedValues = [rvReadTimeout]
    Client = RESTClient1
    Params = <>
    Response = RESTResponse1
    ReadTimeout = 3000
    SynchronizedEvents = False
    Left = 180
    Top = 24
  end
  object NetHTTPClient1: TNetHTTPClient
    UserAgent = 'Embarcadero URI Client/1.0'
    Left = 84
    Top = 161
  end
  object NetHTTPRequest1: TNetHTTPRequest
    MethodString = 'GET'
    Client = NetHTTPClient1
    Left = 356
    Top = 161
  end
  object RESTResponse1: TRESTResponse
    Left = 324
    Top = 24
  end
end
