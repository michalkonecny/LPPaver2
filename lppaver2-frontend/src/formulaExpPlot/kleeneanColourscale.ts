import { getTruthColour } from "@/steps/stepsStore";

export function getKleeneanColourscale(
  intensity: number[],
  middleIntensity: number,
): Plotly.ColorScale {
  const minIntensity = Math.min(...intensity);
  const maxIntensity = Math.max(...intensity);
  const trueColour = getTruthColour("CertainTrue");
  const falseColour = getTruthColour("CertainFalse");
  const unknownColour = getTruthColour("TrueOrFalse");
  if (minIntensity > middleIntensity) {
    // all values are CertainTrue
    return [
      [0, trueColour],
      [1, trueColour],
    ];
  } else if (maxIntensity < middleIntensity) {
    // all values are CertainFalse
    return [
      [0, falseColour],
      [1, falseColour],
    ];
  } else if (
    minIntensity === middleIntensity &&
    maxIntensity === middleIntensity
  ) {
    // all values are TrueOrFalse
    return [
      [0, unknownColour],
      [1, unknownColour],
    ];
  } else if (minIntensity >= middleIntensity) {
    // all values are CertainTrue or TrueOrFalse, and both are present
    return [
      [0, unknownColour],
      [1, trueColour],
    ];
  } else if (maxIntensity <= middleIntensity) {
    // all values are CertainFalse or TrueOrFalse, and both are present
    return [
      [0, falseColour],
      [1, unknownColour],
    ];
  } else {
    // values of all three types are present
    const unknownRatio =
      (middleIntensity - minIntensity) / (maxIntensity - minIntensity);
    return [
      [0, falseColour],
      [unknownRatio, unknownColour],
      [1, trueColour],
    ];
  }
}

