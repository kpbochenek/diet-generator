import Html exposing (..)
import Html.App as App
import Html.Attributes as H exposing (..)
import Html.Events exposing (onClick, onInput, on, targetValue)
import Json.Decode as Json
import String

main =
    App.beginnerProgram {
        model = initModel,
        view = view,
        update = update
    }

type alias Model =
    { calories : Int
    , protein : Int
    , carbohydrates : Int
    , fat : Int
    , meals : Int
    }

initModel : Model
initModel =
  { calories = 1500
  , protein = 20
  , carbohydrates = 60
  , fat = 20
  , meals = 4
  }

view model =
  div []
  [ div []
      [ text "kalorie: "
        , input
          [ required True
          , type' "text"
          , placeholder "Kalorie"
          , value (toString model.calories)
          , onInput UpdateCalories
          ] []
      ]
    , div [] [
        div [] [
          text "Bialko: "
          , input [ type' "range"
                  , H.min "0"
                  , H.max "100"
                  , H.value (toString model.protein)
                  , onInput UpdateProtein
                  ] []
          , text <| displayPercent model.protein
        ]
        , div [] [
            text "Weglowodany: "
            , input [ type' "range"
                    , H.min "0"
                    , H.max "100"
                    , H.value (toString model.carbohydrates)
                    , onInput UpdateCarbohydrates
                    ] []
            , text <| displayPercent model.carbohydrates
        ]
        , div [] [
            text "Tluszcze: "
            , input [ type' "range"
                    , H.min "0"
                    , H.max "100"
                    , onInput UpdateFat
                    ] []
            , text <| displayPercent model.fat
        ]
    ]
    , div [ warningWhenNotSum(model) ]
      [ text ("Proporcje nie sumuja sie do 100% --> " ++ (toString (model.protein + model.carbohydrates + model.fat))) ]
    , div []
      [ text "Ilosc posilkow: "
      , select [on "change" (Json.map UpdateMeals handleSelectHtml) ]
          [ option [selected (model.meals == 3)] [text "3"]
          , option [selected (model.meals == 4)] [text "4"]
          , option [selected (model.meals == 5)] [text "5"]
          ]
      , hr [] []
      , text "Zapotrzebowanie na substancje odżywcze: "
      , div []
          [ div [] [ text "Bialko: ", text (displayGram (model.calories * model.protein // 4 // 100)) ]
          , div [] [ text "Weglowodany: ", text (displayGram (model.calories * model.carbohydrates // 4 // 100)) ]
          , div [] [text "Tluszcze: ", text (displayGram (model.calories * model.fat // 9 // 100)) ]
          ]
      , hr [] []
      , div []
        [ button [ onClick GenerateDiet ] [ text "Generuj plan" ]
        , button [ onClick Reset ] [ text "Resetuj" ]
        ]
      ]
    , hr [] []
  ]

warningWhenNotSum : Model -> Attribute t
warningWhenNotSum model = case checkWarning model of
  True -> style [ ("backgroundColor", "red") ]
  False -> style [ ("visibility", "hidden") ]

handleSelectHtml : Json.Decoder Int
handleSelectHtml = targetValue `Json.andThen` \val ->
  case String.toInt val of
    Ok i -> Json.succeed i
    Err err -> Json.fail err

displayGram : Int -> String
displayGram v = (toString v) ++ " g"

displayPercent : Int -> String
displayPercent v = (toString v) ++ " %"

convertToInt : String -> Int
convertToInt v = Result.withDefault 0 (String.toInt v)

checkWarning : Model -> Bool
checkWarning model = model.protein + model.carbohydrates + model.fat /= 100

type Msg =
    UpdateCalories String
  | UpdateProtein String
  | UpdateCarbohydrates String
  | UpdateFat String
  | UpdateMeals Int
  | GenerateDiet
  | Reset

update msg model =
    case msg of
        UpdateCalories newCalories -> {model | calories = convertToInt newCalories}
        UpdateProtein newProtein -> {model | protein = convertToInt newProtein}
        UpdateCarbohydrates newCarbohydrates -> {model | carbohydrates = convertToInt newCarbohydrates}
        UpdateFat newFat -> {model | fat = convertToInt newFat}
        UpdateMeals newMeals -> {model | meals = newMeals}
        GenerateDiet -> model
        Reset -> initModel