# purescript-servant
A servant like DSL for putting types to REST APIs

## Intro
This library defines a type level DSL à la servant to describe the types of http requests and to implement client libraries. The goal is to be able to make integrating client APIs into your code easy, formulaic, and type-safe. At the moment there is no support for something like `servent-server`, although it's certainly possible to build something like that on top of `purescript-express` using the combinators in this library.

## Examples
The test suite is also meant to serve as a primary example. It's supposed to simulate a simple server for hosting photos, with the client library written using `purescript-servant`. Let's look at the client portion for posting public photos. Assume that the server's documentation looked something like:


```
ROUTE:
POST /photos/public

BODY:
Type: Json
Format:
  { username : string
  , title : string
  , data : hex
  }

SUCCESS RESPONSE:
Code: 200
Type: Json
Content: { photoID : integer }
```

First we must declare the types for the photo object. Notice is that the `photoID` is assigned by the server and given as a response to the POST request. We model the API types like:

```purescript
newtype Username = Username String

newtype PhotoID = PhotoID Int

newtype PostPhotoBody =
  PostPhotoBody { username :: Username
                , title :: String
                , data :: ByteString 
                }
            
instance encodeJsonPostPhotoBody :: EncodeJson PostPhotoBody where
  ...

newtype PostPhotoResponse =
  PostPhotoResponse { photoID :: PhotoID }

instance decodeJsonPostPhotoResponse :: DecodeJson PostPhotoResponse where
  ...
```

Notice that we have to declare `Json` codecs for our route types because the documentation is specifying that it want `Json` for everything. Using the route documentation and looking through the servant combinators, we can then model this route like

```
type PostPublicPhoto =
  S "photos" :> S "public" :> Body Json PostPhotoBody :> GET Json PostPhotoResponse

```

We want to turn this route into a simple function that our client can use, which is what the `makeClientRoute` function allows us to do. Note that in order to give an explicit type to this function, you need to know how to mentally convert the route type into a function type. This process is straight forward, but takes getting used to. In this case it turns out to look like:

```purescript
module PhotoAppClient.Routes where

postPublicPhoto
  :: forall m.
     RunClient m
  => MonadError AjaxError m
  => PostPhotoBody
  -> m PostPhotoResponse
postPublicPhoto = makeClientRoute (RouteProxy :: RouteProxy PostPublicPhoto)
```

The `RunClient` typeclass parameterizes `m`s that can make requests and has only one method to implement

```purescript
class RunClient m where
  runRequest :: forall a. Affjax.Request a -> m (Affjax.Response a)
```

The prototypical example of an `m` would be something like 

```purescript
newtype ClientM a =
  ClientM (ReaderT ClientEnv (ExceptT AjaxError Aff) a) 

instance runClientClientM :: RunRequest ClientM where
  runRequest = defaultRunRequest
```

where `defaultRunRequest` is defined in `Servant.Client.Request`.

Now we have all we need in order to write our post function. Since our application will often want to reference the photo and the photoID together, we will also create a separate application type for the photo object:

```purescript

import PhotoAppClient.Routes as Routes

newtype Photo =
  Photo { username :: Username
        , title :: String
        , data :: ByteString
        , photoID :: PhotoID
        }

postPublicPhoto
  :: { username :: Username
     , title :: String
     , data :: ByteString
     }
  -> ClientM Photo
postPublicPhoto photo@{username, title, data} = do
  let body = PostPhotoBody photo
  PostPhotoResponse {photoID} <- Routes.postPublicPhoto body
  pure $ Photo {username, title, data, photoID}

```

For a more comprehensive picture of this example and other routes relevant to the photo app, see the tests.

## Combinators

We have defined several combinators which are all exported from `Servant.API`, but of course anyone is welcome to suggest additions to this list. For example `servant` has combinators for such things as basic auth and response headers, which we have left unimplemented. In order to implement these features it's as simple as implementing the `HasClient` instances for the combinators.
