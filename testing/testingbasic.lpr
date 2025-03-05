program testingbasic;

type
  FloatArray = array of Double;

function CalculateMean(arr: FloatArray): Double;
var
  i: Integer;
  sum: Double;
begin
  sum := 0;
  if Length(arr) = 0 then
    CalculateMean := 0.0 // Or raise an exception, depending on your desired behavior
  else
  begin
    for i := Low(arr) to High(arr) do
      sum := sum + arr[i];
    CalculateMean := sum / Length(arr);
  end;
end;

procedure RunTest(arr: FloatArray; expected: Double; testName: String);
var
  actual: Double;
begin
  actual := CalculateMean(arr);
  if Abs(actual - expected) < 1e-9 then // Use a tolerance for floating-point comparisons
    Writeln(testName, ': Passed')
  else
    Writeln(testName, ': Failed. Expected ', expected:0:4, ', but got ', actual:0:4);
end;

var
  emptyArray: FloatArray;
  singleElementArray: FloatArray;
  positiveArray: FloatArray;
  negativeArray: FloatArray;
  mixedArray: FloatArray;
  largeValuesArray: FloatArray;
  arrayWithZeroes: FloatArray;

begin
  // Test Cases

  // 1. Empty array
  SetLength(emptyArray, 0);
  RunTest(emptyArray, 0.0, 'Empty Array');

  // 2. Single element array
  SetLength(singleElementArray, 1);
  singleElementArray[0] := 5.0;
  RunTest(singleElementArray, 5.0, 'Single Element');

  // 3. Array with positive numbers
  SetLength(positiveArray, 3);
  positiveArray[0] := 1.0;
  positiveArray[1] := 2.0;
  positiveArray[2] := 3.0;
  RunTest(positiveArray, 2.0, 'Positive Numbers');

  // 4. Array with negative numbers
  SetLength(negativeArray, 3);
  negativeArray[0] := -1.0;
  negativeArray[1] := -2.0;
  negativeArray[2] := -3.0;
  RunTest(negativeArray, -2.0, 'Negative Numbers');

  // 5. Array with mixed positive and negative numbers
  SetLength(mixedArray, 4);
  mixedArray[0] := -1.0;
  mixedArray[1] := 2.0;
  mixedArray[2] := -3.0;
  mixedArray[3] := 4.0;
  RunTest(mixedArray, 0.5, 'Mixed Numbers');

  // 6. Array with large values
  SetLength(largeValuesArray, 2);
  largeValuesArray[0] := 1e9;
  largeValuesArray[1] := 2e9;
  RunTest(largeValuesArray, 1.5e9, 'Large Values');

  // 7. array containing zeroes
  SetLength(arrayWithZeroes, 4);
  arrayWithZeroes[0] := 0;
  arrayWithZeroes[1] := 1;
  arrayWithZeroes[2] := 2;
  arrayWithZeroes[3] := 0;
  RunTest(arrayWithZeroes, 0.75, 'Array with zeroes');

end.
