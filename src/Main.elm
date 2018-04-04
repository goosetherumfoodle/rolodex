port module Main exposing (..)

import Html exposing (..)
import Html.Attributes
    exposing
        ( src
        , type_
        , placeholder
        , value
        , style
        , class
        , for
        , id
        )
import Html.Events exposing (onInput, onSubmit, onClick)
import Time exposing (second)
import Http
import Json.Decode exposing (map3, list, field, string)
import Json.Encode
import Process
import Task exposing (attempt)
import Bootstrap.CDN as CDN


-- todos:
-- only generate valid number on submission?
-- re-insert as-you-type feedback
-- on-the-fly field validation?
-- (better) sorting
-- searching
-- extract modules
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
    , numberError : Maybe String
    , nameError : Maybe String
    , contextError : Maybe String
    }


init : ( Model, Cmd Msg )
init =
    ( emptyModel, getContacts )


emptyModel : Model
emptyModel =
    { newContact = emptyNewContact
    , contacts = []
    , feedback = Nothing
    }


emptyNewContact : NewContact
emptyNewContact =
    { name = ""
    , context = ""
    , rawNumber = ""
    , countryCode = "US"
    , ppNumber = ""
    , validNumber = Nothing
    , numberError = Nothing
    , nameError = Nothing
    , contextError = Nothing
    }


emptyContact : Contact
emptyContact =
    { name = "", context = "", number = "" }



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
    | SortContacts (Contact -> String)


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
            case validateSubmission model of
                Ok ( validatedModel, contact ) ->
                    ( { validatedModel | contacts = contact :: model.contacts }
                    , postContact contact
                    )

                Err modelWithErrors ->
                    ( modelWithErrors, Cmd.none )

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

        SortContacts attribute ->
            ( { model | contacts = List.sortBy attribute model.contacts }
            , Cmd.none
            )


removeContact : Contact -> List Contact -> List Contact
removeContact query =
    List.filter <| \elem -> elem.number /= query.number


validateSubmission : Model -> Result Model ( Model, Contact )
validateSubmission m =
    let
        validated =
            validateContext
                (validateName
                    (validateNumber
                        ( (resetFeedback m)
                        , emptyContact
                        )
                    )
                )
    in
        if hasSubmissionErrors (Tuple.first validated) then
            Err (Tuple.first validated)
        else
            Ok validated


validateContext : ( Model, Contact ) -> ( Model, Contact )
validateContext ( m, c ) =
    let
        newContact =
            m.newContact
    in
        if (String.length m.newContact.context) < 1 then
            let
                updatedNewContact =
                    { newContact | contextError = Just "Context missing" }
            in
                ( { m
                    | feedback = badSubmissionFeedback
                    , newContact = updatedNewContact
                  }
                , c
                )
        else
            let
                updatedNewContact =
                    { newContact | contextError = Nothing }
            in
                ( { m | newContact = updatedNewContact }
                , { c | context = newContact.context }
                )


hasSubmissionErrors : Model -> Bool
hasSubmissionErrors m =
    case m.feedback of
        Just _ ->
            True

        Nothing ->
            False


resetFeedback : { c | feedback : b } -> { c | feedback : Maybe a }
resetFeedback m =
    { m | feedback = Nothing }


validateName : ( Model, Contact ) -> ( Model, Contact )
validateName ( m, c ) =
    let
        newContact =
            m.newContact
    in
        if (String.length m.newContact.name) < 1 then
            let
                updatedNewContact =
                    { newContact | nameError = Just "Name missing" }
            in
                ( { m
                    | feedback = badSubmissionFeedback
                    , newContact = updatedNewContact
                  }
                , c
                )
        else
            let
                updatedNewContact =
                    { newContact | nameError = Nothing }
            in
                ( { m | newContact = updatedNewContact }
                , { c | name = newContact.name }
                )


validateNumber : ( Model, Contact ) -> ( Model, Contact )
validateNumber modelContact =
    let
        results =
            Result.andThen validateDuplicates <| validateNumberFormat modelContact
    in
        case results of
            Ok res ->
                res

            Err res ->
                res


validateNumberFormat : ( Model, Contact ) -> Result ( Model, Contact ) ( String, Model, Contact )
validateNumberFormat ( m, c ) =
    let
        newContact =
            m.newContact
    in
        case m.newContact.validNumber of
            Nothing ->
                let
                    updatedNewContact =
                        { newContact | numberError = Just "Invalid number" }
                in
                    Err
                        ( { m
                            | feedback = badSubmissionFeedback
                            , newContact = updatedNewContact
                          }
                        , c
                        )

            Just validNumber ->
                let
                    updatedNewContact =
                        { newContact | numberError = Nothing }
                in
                    Ok ( validNumber, m, c )


validateDuplicates : ( String, Model, Contact ) -> Result ( Model, Contact ) ( Model, Contact )
validateDuplicates ( newNumber, m, c ) =
    let
        contacts =
            m.contacts

        hasNumber contact =
            contact.number == newNumber

        dupes =
            List.filter hasNumber contacts

        firstDupe =
            List.head dupes
    in
        case firstDupe of
            Nothing ->
                Ok ( m, { c | number = newNumber } )

            Just _ ->
                let
                    newContact =
                        m.newContact

                    updatedContact =
                        { newContact | numberError = Just "We already have a contact with this number" }
                in
                    Err
                        ( { m
                            | feedback = badSubmissionFeedback
                            , newContact = updatedContact
                          }
                        , c
                        )


badSubmissionFeedback : Maybe (Result String String)
badSubmissionFeedback =
    Just (Err "Please fix errors in form")



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


contactsTable : List Contact -> Html Msg
contactsTable contacts =
    div [ class "card mt-3" ]
        [ div [ class "card-header" ] [ h2 [] [ text "Contacts list" ] ]
        , div [ class "card-body" ]
            [ table [ class "table" ]
                (tr []
                    [ th [ onClick <| SortContacts .name ] [ text "Name" ]
                    , th [ onClick <| SortContacts .context ] [ text "Context" ]
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
                    [ nameInput contact
                    , contextInput contact
                    , numberInput contact
                    , div [ class "col" ]
                        [ select [] [ option [] [ text contact.countryCode ] ]
                        , button
                            [ type_ "submit", class "btn btn-primary" ]
                            [ text "Save" ]
                        ]
                    ]
                ]
            ]
        ]


type alias InputDivAttrs =
    { name : String
    , example : String
    , errorMsg : String
    , constructor : String -> Msg
    , valueAccessor : NewContact -> String
    , errorAccessor : NewContact -> Maybe String
    }


contextInput : NewContact -> Html Msg
contextInput =
    attrInput
        { name = "Context"
        , example = "Work, School, etc"
        , errorMsg = ""
        , constructor = Context
        , valueAccessor = .context
        , errorAccessor = .contextError
        }


nameInput : NewContact -> Html Msg
nameInput =
    attrInput
        { name = "Name"
        , example = "Jenny"
        , errorMsg = ""
        , constructor = Name
        , valueAccessor = .name
        , errorAccessor = .nameError
        }


numberInput : NewContact -> Html Msg
numberInput =
    attrInput
        { name = "Number"
        , example = "(555) 555-5555"
        , errorMsg = ""
        , constructor = RawNumber
        , valueAccessor = .rawNumber
        , errorAccessor = .numberError
        }


attrInput : InputDivAttrs -> NewContact -> Html Msg
attrInput attrs contact =
    case attrs.errorAccessor contact of
        Just errorMsg ->
            errorInputDiv { attrs | errorMsg = errorMsg } contact

        Nothing ->
            validInputDiv attrs contact


errorInputDiv : InputDivAttrs -> NewContact -> Html Msg
errorInputDiv attrs =
    inputDiv "is-invalid"
        (div
            [ class "invalid-feedback" ]
            [ text attrs.errorMsg ]
        )
        attrs


validInputDiv : InputDivAttrs -> NewContact -> Html Msg
validInputDiv =
    inputDiv "" emptyDiv


emptyDiv : Html msg
emptyDiv =
    div [] []


inputDiv : String -> Html Msg -> InputDivAttrs -> NewContact -> Html Msg
inputDiv errorClass errorDiv attrs contact =
    let
        inputId =
            "newContact" ++ attrs.name
    in
        div [ class "form-group col" ]
            [ label [ for inputId ] [ text attrs.name ]
            , input
                [ class <| String.join " " [ "form-control", errorClass ]
                , type_ "text"
                , placeholder attrs.example
                , value <| attrs.valueAccessor contact
                , onInput attrs.constructor
                , id inputId
                ]
                []
            , errorDiv
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
