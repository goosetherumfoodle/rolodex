port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (src, type_, placeholder, value, style, class, for, id)
import Html.Events exposing (onInput, onSubmit, onClick)
import Time exposing (second)
import Http
import Json.Decode exposing (map3, list, field, string)
import Json.Encode
import Process
import Task exposing (attempt)
import Bootstrap.CDN as CDN


---- PORTS ----


port toPrettyPrinter : List String -> Cmd msg


port fromPrettyPrinter : (String -> msg) -> Sub msg


port toFormatIfValid : List String -> Cmd msg


port fromFormatIfValid : (Maybe String -> msg) -> Sub msg



---- MODEL ----


type alias Model =
    { newContact : NewContact
    , contacts : List Contact
    , feedback : Maybe (Result String String)
    }


type alias Contact =
    { name : String, context : String, number : String }


type alias NewContact =
    { name : String
    , context : String
    , rawNumber : String
    , countryCode : String
    , ppNumber : String
    , validNumber : Maybe String
    }


init : ( Model, Cmd Msg )
init =
    ( emptyModel, getContacts )


emptyModel : Model
emptyModel =
    { newContact = emptyContact
    , contacts = []
    , feedback = Nothing
    }


emptyContact : NewContact
emptyContact =
    { name = ""
    , context = ""
    , rawNumber = ""
    , countryCode = "US"
    , ppNumber = ""
    , validNumber = Nothing
    }



---- UPDATE ----


type Msg
    = Name String
    | Context String
    | RawNumber String
    | Submit
    | Error String
    | Success String
    | Delete Contact
    | ClearFeedback
    | Contacts (List Contact)
    | PrettyPrintNumber String
    | ValidNumber (Maybe String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Name name ->
            let
                newContact =
                    model.newContact

                updatedContact =
                    { newContact | name = name }
            in
                ( { model | newContact = updatedContact }
                , Cmd.none
                )

        Context context ->
            let
                newContact =
                    model.newContact

                updatedContact =
                    { newContact | context = context }
            in
                ( { model | newContact = updatedContact }
                , Cmd.none
                )

        RawNumber number ->
            let
                newContact =
                    model.newContact

                updatedContact =
                    { newContact | rawNumber = number }
            in
                ( { model | newContact = updatedContact }
                , Cmd.batch
                    [ toPrettyPrinter [ newContact.countryCode, number ]
                    , toFormatIfValid [ newContact.countryCode, number ]
                    ]
                )

        Submit ->
            case model.newContact.validNumber of
                Nothing ->
                    update (Error "Please enter a valid phone number") model

                Just validNumber ->
                    let
                        contact =
                            { name = model.newContact.name
                            , context = model.newContact.context
                            , number = validNumber
                            }
                    in
                        ( { model
                            | newContact = emptyContact
                            , contacts = contact :: model.contacts
                          }
                        , postContact contact
                        )

        Error msg ->
            ( { model | feedback = Just (Err msg) }
            , Cmd.none
            )

        Success msg ->
            ( { model | feedback = Just (Ok msg) }
            , attempt (always ClearFeedback) (Process.sleep (second * 5))
            )

        Delete contact ->
            ( { model | contacts = removeContact contact model.contacts }
            , deleteContact contact
            )

        ClearFeedback ->
            ( { model | feedback = Nothing }
            , Cmd.none
            )

        Contacts contacts ->
            ( { model | contacts = contacts }, Cmd.none )

        PrettyPrintNumber pp ->
            let
                newContact =
                    model.newContact

                updatedContact =
                    { newContact | ppNumber = pp }
            in
                ( { model | newContact = updatedContact }, Cmd.none )

        ValidNumber valid ->
            let
                newContact =
                    model.newContact

                updatedContact =
                    { newContact | validNumber = valid }
            in
                ( { model | newContact = updatedContact }, Cmd.none )


removeContact : Contact -> List Contact -> List Contact
removeContact query =
    List.filter (\elem -> elem.number /= query.number)



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ fromPrettyPrinter PrettyPrintNumber
        , fromFormatIfValid ValidNumber
        ]



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ CDN.stylesheet
        , userFeedback model.feedback
        , div []
            [ newContactForm model.newContact
            , contactsTable model.contacts
            ]
        ]


userFeedback : Maybe (Result String String) -> Html Msg
userFeedback feedback =
    case feedback of
        Nothing ->
            div [ class "alert invisible" ] [ text "invisible div" ]

        Just (Err msg) ->
            div [ class "alert alert-danger" ] [ text msg ]

        Just (Ok msg) ->
            div [ class "alert alert-success" ] [ text msg ]


contactsTable contacts =
    div [ class "card mt-3" ]
        [ div [ class "card-header" ] [ h2 [] [ text "Contacts list" ] ]
        , div [ class "card-body" ]
            [ table [ class "table" ]
                (tr []
                    [ th [] [ text "Name" ]
                    , th [] [ text "Context" ]
                    , th [] [ text "Phone" ]
                    , th [] [ text "Actions" ]
                    ]
                    :: contactRows contacts
                )
            ]
        ]


contactRows : List Contact -> List (Html Msg)
contactRows =
    let
        contactRow contact =
            tr []
                [ td [] [ text contact.name ]
                , td [] [ text contact.context ]
                , td [] [ text contact.number ]
                , td []
                    [ span [ onClick (Delete contact) ]
                        [ i [ class "fa fa-trash" ] [] ]
                    ]
                ]
    in
        List.map contactRow


newContactForm : NewContact -> Html Msg
newContactForm contact =
    div [ class "card" ]
        [ div [ class "card-header" ]
            [ h2 [] [ text "New Contact" ] ]
        , div [ class "card-body" ]
            [ form
                [ onSubmit Submit ]
                [ div [ class "form-row" ]
                    [ div [ class "col" ] [ inputField "name" "Jenny" Name contact.name ]
                    , div [ class "col" ] [ inputField "context" "Work, School, Etc" Context contact.context ]
                    , div [ class "form-group col" ]
                        [ label [ for "newContactNumber" ] [ text "Number" ]
                        , input
                            [ class "form-control"
                            , type_ "text"
                            , placeholder "(555) 555-5555"
                            , value contact.rawNumber
                            , onInput RawNumber
                            , id "newContactNumber"
                            ]
                            []
                        , div [ class "small" ] [ text contact.ppNumber ]
                        ]
                    , div [ class "col" ]
                        [ select [] [ option [] [ text contact.countryCode ] ]
                        , button [ type_ "submit", class "btn btn-primary" ] [ text "Save" ]
                        ]
                    ]
                ]
            ]
        ]


inputField : String -> String -> (String -> msg) -> String -> Html msg
inputField tagId dummy action val =
    div [ class "form-group" ]
        [ label [ for tagId ] [ text tagId ]
        , input
            [ class "form-control"
            , type_ "text"
            , placeholder dummy
            , value val
            , onInput action
            , id tagId
            ]
            []
        ]



-- HTTP --


deleteContact : Contact -> Cmd Msg
deleteContact contact =
    let
        handler resp =
            case resp of
                Ok _ ->
                    Success "Successfully deleted contact"

                Err _ ->
                    Error "Error deleting contact"

        contactJson =
            Json.Encode.object
                [ ( "id", Json.Encode.string contact.number )
                ]
    in
        Http.send handler
            (Http.request
                { method = "DELETE"
                , headers = []
                , url = "http://localhost:3001/contacts/" ++ contact.number
                , body = Http.emptyBody
                , expect = Http.expectStringResponse (\_ -> Ok ())
                , timeout = Nothing
                , withCredentials = False
                }
            )


postContact : Contact -> Cmd Msg
postContact contact =
    let
        handler resp =
            case resp of
                Ok _ ->
                    Success "Successfully stored contact"

                Err _ ->
                    Error "Error posting contact"

        contactJson =
            Json.Encode.object
                [ ( "id", Json.Encode.string contact.number )
                , ( "name", Json.Encode.string contact.name )
                , ( "number", Json.Encode.string contact.number )
                , ( "context", Json.Encode.string contact.context )
                ]
    in
        Http.send handler
            (Http.post "http://localhost:3001/contacts"
                (Http.jsonBody contactJson)
                Json.Decode.value
            )


getContacts : Cmd Msg
getContacts =
    let
        handler resp =
            case resp of
                Ok contacts ->
                    Contacts contacts

                Err _ ->
                    Error "Error connecting to server"
    in
        Http.send handler
            (Http.get "http://localhost:3001/contacts"
                (list
                    (map3 Contact
                        (field "name" string)
                        (field "context" string)
                        (field "number" string)
                    )
                )
            )



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
