import type { Var } from "@/formulas/exprs";
import type { Box } from "@/steps/steps";

export function pickXY(box: Box): { xVar: Var; yVar: Var } {
  const vars = Object.keys(box.box_.varDomains ?? {});

  // find vars that start with 'x' and 'y'
  const xVars = vars.filter(v => v.toLowerCase().startsWith("x"));
  const yVars = vars.filter(v => v.toLowerCase().startsWith("y"));

  // pick xVar and yVar, preferring those that start with 'x' and 'y'
  let xVar = (xVars.length == 1 ? xVars[0] : vars[0]) ?? "_x";
  let yVar = (yVars.length == 1 ? yVars[0] : vars[1]) ?? "_y";

  // ensure xVar and yVar are different if possible
  if (xVar === yVar) {
    yVar = vars.find(v => v !== xVar) || "_y";
  }
  return { xVar, yVar };
}
