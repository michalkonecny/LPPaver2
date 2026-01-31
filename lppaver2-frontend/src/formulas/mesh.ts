import type { Interval } from "@/steps/steps";
import _ from "lodash";

export type DomainAndN = { domain: Interval, n: number };

export function getDomainSamples(params: DomainAndN): { withEndpoints: number[], midpoints: number[] } {
  const { domain, n } = params;
  if (n <= 1) {
    throw new Error("n must be at least 2 to get samples with endpoints and midpoints.");
  }
  const withEndpoints = _.range(domain.l, domain.u + 1e-10, (domain.u - domain.l) / (n - 1));
  const midpoints = withEndpoints.slice(0, -1).map((v, i) => (v + withEndpoints[i + 1]!) / 2);
  return { withEndpoints, midpoints };
}

export type Triangulation2D = {
  x: number[]; // x coordinates of points
  y: number[]; // y coordinates of points
  i: Plotly.TypedArray; // indices of triangle vertices in x,y arrays
  j: Plotly.TypedArray;
  k: Plotly.TypedArray;
};

export function getHexTriangulation(xDomainAndN: DomainAndN, yDomainAndN: DomainAndN): Triangulation2D {
  const xSamples = getDomainSamples(xDomainAndN);
  const ySamples = getDomainSamples(yDomainAndN);

  const xFullRowLength = xSamples.withEndpoints.length;
  const xMidRowLength = xSamples.midpoints.length;

  const x: number[] = [];
  const y: number[] = [];
  const i: number[] = [];
  const j: number[] = [];
  const k: number[] = [];

  // iterate over y samples, alternating full rows and midpoint rows
  ySamples.withEndpoints.forEach((yVal, yIdx) => {
    if (yIdx % 2 === 0) {
      // even row: full x samples
      // add points for all x samples with endpoints
      xSamples.withEndpoints.forEach((xVal, xIdx) => {
        x.push(xVal);
        y.push(yVal);
      });
    } else {
      // odd row: midpoints only
      xSamples.midpoints.forEach((xVal, xIdx) => {
        x.push(xVal);
        y.push(yVal);

        /*
            Create triangles connecting this midpoint row to the rows above and below:

            Prev Full Row :  7----(8)----9-----10
                              \   / \ x / \   /
                               \ /   \ / x \ /
            Midpoint Row  :     4---((5))---6
                               / \   / \ x / \ 
                              /   \ / x \ /   \
            Next Full Row :  0----(1)----2-----3
        */
        const currentPtIdx = x.length - 1;     // ((5)) in diagram
        const prevRowPt1Idx = currentPtIdx - xFullRowLength; // (1) in diagram
        const nextRowPt1Idx = currentPtIdx + xMidRowLength;  // (8) in diagram

        // triangles with one vertex at current midpoint row
        // triangle to prev row
        i.push(prevRowPt1Idx);      // (1) in diagram
        j.push(currentPtIdx);       // ((5)) in diagram
        k.push(prevRowPt1Idx + 1);  // 2 in diagram
        // triangle to next row (if present)
        if (yIdx < ySamples.withEndpoints.length - 1) {
          i.push(nextRowPt1Idx);      // (8) in diagram
          j.push(currentPtIdx);       // ((5)) in diagram
          k.push(nextRowPt1Idx + 1);  // 9 in diagram
        }

        // if not the last midpoint in the row, create triangles with two vertices at current midpoint row
        if (xIdx < xSamples.midpoints.length - 1) {
          // triangle to the right towards the previous row
          i.push(currentPtIdx);       // ((5)) in diagram
          j.push(currentPtIdx + 1);   // 6 in diagram
          k.push(prevRowPt1Idx + 1);  // 2 in diagram
          // triangle to the right towards the next row (if present)
          if (yIdx < ySamples.withEndpoints.length - 1) {
            i.push(currentPtIdx);       // ((5)) in diagram
            j.push(currentPtIdx + 1);   // 6 in diagram
            k.push(nextRowPt1Idx + 1);  // 9 in diagram
          }
        }
      });
    }
  });

  return { x, y, i: new Int32Array(i), j: new Int32Array(j), k: new Int32Array(k) };
}



// function allCombinations(varValues: Record<Var, number[]>): Record<Var, number>[] {
//   // process the variables one by one, building up combinations
//   _.reduce
//   return Object.entries(varValues).reduce(
//     // reduce iterator
//     (soFarCombinations: Record<Var, number>[], nextVarAndItsValues: [Var, number[]]) =>
//       // for each combination so far, add each value of the next variable, give new combinations
//       soFarCombinations.flatMap((soFarCombination: Record<Var, number>) =>
//         nextVarAndItsValues[1].map((value: number) =>
//           // create new combination with next variable added
//           ({ ...soFarCombination, [nextVarAndItsValues[0]]: value })))
//     ,
//     // initial value
//     [{} as Record<Var, number>] // no variables yet, so one empty combination
//   );
// }
