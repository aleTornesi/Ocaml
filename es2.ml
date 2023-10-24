type scale = Celsius | Kelvin | Fahrenheit | Rankine | Delisle | Newton | Reaumur | Romer;;
type conversionTable =  {
  celsius: float;
  kelvin: float;
  fahrenheit: float;
  rankine: float;
  delisle: float;
  newton: float;
  reaumur: float;
  romer: float
};;

let getConversionTable value = {
  celsius = value -. 273.15;
  kelvin = value;
  fahrenheit = value *. 1.8 -. 459.67;
  rankine = value *. 1.8;
  delisle = (373.15 -. value) *. 1.5;
  newton = (value -. 273.15) *. 0.33;
  reaumur = (value -. 273.15) *. 0.8;
  romer = (value -. 273.15) *. 0.525 +. 7.5
}
let conversionTableToList conversionTable = [conversionTable.celsius, conversionTable.kelvin, conversionTable.fahrenheit, conversionTable.rankine, conversionTable.delisle, conversionTable.newton, conversionTable.reaumur, conversionTable.romer]

let removeAt x list = 
  let rec removeAt i = function 
    [] -> []
    | h::l -> if (i == x) then l else h::removeAt (i + 1) l
in removeAt 0 list


let f temperature = function
  | Celsius -> removeAt 0 (conversionTableToList(getConversionTable temperature))
  | Kelvin -> removeAt 1 (conversionTableToList(getConversionTable temperature))
  | Fahrenheit -> removeAt 2 (conversionTableToList(getConversionTable temperature))
  | Rankine -> removeAt 3 (conversionTableToList(getConversionTable temperature))
  | Delisle -> removeAt 4 (conversionTableToList(getConversionTable temperature))
  | Newton -> removeAt 5 (conversionTableToList(getConversionTable temperature))
  | Reaumur -> removeAt 6 (conversionTableToList(getConversionTable temperature))
  | Romer -> removeAt 7 (conversionTableToList(getConversionTable temperature))
  

 
