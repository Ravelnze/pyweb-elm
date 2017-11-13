module BootstrapLayout exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Navigation exposing (Location)
import UrlParser exposing ((</>))
import Bootstrap.CDN as CDN
import Bootstrap.Navbar as Navbar
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Modal as Modal
import Bootstrap.Carousel as Carousel exposing (defaultStateOptions)
import Bootstrap.Carousel.Slide as Slide
import Bootstrap.Form as Form
import DatePicker exposing (defaultSettings)


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { view = view
        , update = update
        , subscriptions = subscriptions
        , init = init
        }


type alias Model =
    { page : Page
    , navState : Navbar.State
    , firstName : String
    , lastName : String
    , email : String
    , phone : Int
    , venue : String
    , comments : String
    }


type Page
    = Home
    | Technologies
    | Contact
    | NotFound


init : Location -> ( Model, Cmd Msg )
init location =
    let
        ( navState, navCmd ) =
            Navbar.initialState NavMsg

        ( model, urlCmd ) =
            urlUpdate location 
                { navState = navState
                , page = Home
                , firstName = ""
                , lastName = ""
                , email = ""
                , phone = 0
                , venue = ""
                , comments = ""
                }
    in
        ( 
            model, Cmd.batch [ urlCmd, navCmd ] 
        )


type Msg
    = UrlChange Location
    | NavMsg Navbar.State


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
    [ Navbar.subscriptions model.navState NavMsg ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChange location ->
            urlUpdate location model

        NavMsg state ->
            ( { model | navState = state }
            , Cmd.none
            )


urlUpdate : Navigation.Location -> Model -> ( Model, Cmd Msg )
urlUpdate location model =
    case decode location of
        Nothing ->
            ( { model | page = NotFound }, Cmd.none )

        Just route ->
            ( { model | page = route }, Cmd.none )


decode : Location -> Maybe Page
decode location =
    UrlParser.parseHash routeParser location


routeParser : UrlParser.Parser (Page -> a) a
routeParser =
    UrlParser.oneOf
        [ UrlParser.map Home UrlParser.top
        , UrlParser.map Technologies (UrlParser.s "technologies")
        , UrlParser.map Contact (UrlParser.s "contact")
        ]


view : Model -> Html Msg
view model =
    div []
        [ CDN.stylesheet
        , customStyle
        , menu model
        , mainContent model
        ]


customStyle : Html msg
customStyle = 
    node "link"
        [ rel "stylesheet" 
        , href "css/site.css"
        ]
        []


menu : Model -> Html Msg
menu model =
    Navbar.config NavMsg
        |> Navbar.brand 
            [ href "#"
            , title "Totes Tech xPerts"
            ] 
            [ text "Pyweb" ]
        |> Navbar.items
            [ Navbar.itemLink [ href "#" ] [ text "Home" ]
            , Navbar.itemLink [ href "#technologies" ] [ text "Technologies" ]
            , Navbar.itemLink [ href "#contact" ] [ text "Enquiries" ]
            ]
        |> Navbar.view model.navState


mainContent : Model -> Html Msg
mainContent model =
    Grid.containerFluid [] <|
        case model.page of
            Home ->
                pageWelcome model

            Technologies ->
                pageTechnologies model

            Contact ->
                pageContact model

            NotFound ->
                pageNotFound


pageWelcome : Model -> List (Html Msg)
pageWelcome model =
    [ Grid.row []
        [ Grid.col 
            [ Col.attrs 
                [ class "white-bg"
                , class "remove-padding"
                ] 
            ]
            [ img 
                [ class "img-fluid"
                , src "img/python-logo.png"
                ] 
                []
            ]
        ]
    , Grid.row []
        [ Grid.col [ Col.sm1 ] []
        , Grid.col [] 
            [ blockquote 
                [ class "blockquote purple-border" ] 
                [ h3 
                    [ class "upper-pad" ] 
                    [ text "Welcome" ] 
                ] 
            ]
        ]
    , Grid.row []
        [ Grid.col [ Col.xs1, Col.sm2 ] []
        , Grid.col [ Col.xs11, Col.sm8 ]
            [ p 
                [ class "lg-text" ] 
                [ text "PyWeb is a totes young, hip to the hop technology company, ready to make your dreams a reality!... within reason." ] 
            , p 
                [ class "lg-text" ] 
                [ text "We design and build apps for all sorts of platforms, from mobile to web. Full solutions for blah, blah, blah..." ]
            ]
        ]
    ]


pageTechnologies : Model -> List (Html Msg)
pageTechnologies model =
    [ Grid.row []
        [ Grid.col [ Col.sm1 ] []
        , Grid.col [] 
            [ blockquote 
                [ class "blockquote purple-border" ] 
                [ h3 
                    [ class "upper-pad" ] 
                    [ text "Technologies" ] 
                ] 
            ]
        ]
    , Grid.row []
        [ Grid.col [ Col.xs1, Col.sm2 ] []
        , Grid.col [ Col.xs11, Col.sm7, Col.attrs [ class "md-text" ] ]
            [ p 
                [] 
                [ text "PyWeb is renowned for our use of Python in ridiculous ways but we also use other things too!" ] 
            , br [] []
            , p 
                [] 
                [ text "Check them out:" ]
            , ul [] 
                [ li [] [ text "C#" ]
                , li [] [ text "Xamarin" ]
                , li [] [ text "Java" ]
                , li [] [ text "Linux/Mac" ]
                , li [] [ text "Not Windows" ]
                , li [] [ text "node.js" ]
                ]
            , br [] []
            , p 
                [] 
                [ text "And many, many other great technolgies that you don't give a crap about because you just want the damned job done! :D" ]
            ]
        , Grid.col [ Col.sm3 ] 
            [ img 
                [ class "img-fluid rounded-circle" 
                , src "img/reception (2).jpg"
                ] 
                [] 
            ]
        ]
    ]


pageContact : Model -> List (Html Msg)
pageContact model = 
    [ Grid.row []
        [ Grid.col [ Col.sm1 ] []
        , Grid.col [] 
            [ blockquote 
                [ class "blockquote purple-border" ] 
                [ h3 
                    [ class "upper-pad" ] 
                    [ text "Enquiries" ] 
                ] 
            ]
        ]
    , Grid.row []
        [ Grid.col [ Col.xs1, Col.sm2 ] []
        , Grid.col [ Col.xs11, Col.sm6 ]
            [ fieldset
                []
                [ legend 
                    []
                    [ text "Send John an enquiry for an obligation free quote" ]
                , label
                    [ class "control-label" ]
                    [ text "Name" ]
                , Grid.row []
                    [ Grid.col [ Col.sm6, Col.attrs [ class "form-group" ] ]
                        [ input
                            [ id "id_first_name" 
                            , name "first_name" 
                            , required True
                            , placeholder "First Name"
                            , class "input-required textinput textInput form-control"
                            , maxlength 30
                            ]
                            []
                        ]
                    , Grid.col [ Col.sm6, Col.attrs [ class "form-group" ]  ]
                        [ input
                            [ id "id_last_name" 
                            , name "last_name" 
                            , required True
                            , placeholder "Last Name"
                            , class "input-required textinput textInput form-control"
                            , maxlength 30
                            ]
                            []
                        ]
                    ]
                , label 
                    [ class "control-label" ] 
                    [ text "Contact Details" ]
                , Grid.row []
                    [ Grid.col [ Col.sm6, Col.attrs [ class "form-group" ]  ]
                        [ input
                            [ id "id_email_contact" 
                            , name "email_contact" 
                            , required True
                            , placeholder "Email Address"
                            , class "input-required emailinput form-control"
                            , maxlength 254
                            ]
                            []
                        ]
                    , Grid.col [ Col.sm6, Col.attrs [ class "form-group" ]  ]
                        [ input
                            [ id "id_phone_contact" 
                            , name "phone_contact" 
                            , required True
                            , placeholder "Phone Number"
                            , class "textinput form-control"
                            , maxlength 10
                            ]
                            []
                        ]
                    ]
                , label 
                    [ class "control-label" ] 
                    [ text "App Design" ]
                , Grid.row []
                    [ Grid.col [ Col.attrs [ class "form-group" ] ]
                        [ select
                            [ id "id_type" 
                            , name "type"
                            , placeholder ""
                            , class "select form-control"
                            ]
                            [ option [] [text "-- Select Application Type --"]
                            , option [] [text "Mobile App"]
                            , option [] [text "Web App"]
                            , option [] [text "Enterprise App"]
                            , option [] [text "Data Analytics"]
                            ]
                        ]
                    ]
                , label 
                    [ class "control-label" ] 
                    [ text "Comments/Questions" ]
                , Grid.row []
                    [ Grid.col [ Col.attrs [ class "form-group" ] ]
                        [ textarea
                            [ id "id_comments" 
                            , name "comments" 
                            , required True
                            , placeholder "Anything else you would like to know?"
                            , class "textarea form-control"
                            , rows 10
                            , cols 40
                            ]
                            []
                        ]
                    ]
                , Grid.row []
                    [ Grid.col [ Col.attrs [ class "form-group" ] ] 
                        [ div 
                            [ class "buttonHolder" 
                            , type_ "submit"
                            ] 
                            [ button 
                                [ class "btn btn-default" ] 
                                [ text "Submit" ] 
                            ] 
                        ] 
                    ]
                ]
            ]
        , Grid.col [ Col.sm3 ] 
            [ img 
                [ class "img-fluid rounded-circle" 
                , src "img/contact.png" ] 
                []
            ]
        ] 
    ]


pageNotFound : List (Html Msg)
pageNotFound =
    [ Grid.row []
        [ Grid.col [ Col.sm1 ] []
        , Grid.col [] 
            [ blockquote 
                [ class "blockquote purple-border" ] 
                [ h3 
                    [ class "upper-pad" ] 
                    [ text "Not Found" ] 
                ] 
            ]
        ]
    , Grid.row []
        [ Grid.col [ Col.xs1, Col.sm2 ] []
        , Grid.col [ Col.xs11, Col.sm8 ]
            [ p 
                [ class "lg-text lower-pad" ] 
                [ text "Sorry but this page must have been misplaced!" ] 
            ]
        ]
    ]
