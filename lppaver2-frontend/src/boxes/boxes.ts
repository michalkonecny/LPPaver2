import type { Interval } from '@/formulas/evalInfo';

export type Var = string;

export type BoxHash = string;

export type VarDomains = Record<Var, Interval<number>>;

export type Box_ = {
  varDomains: VarDomains;
  splitOrder: Var[];
  volumeVars: Var[];
  except?: VarDomains;
};

export type Box = {
  boxHash: BoxHash;
  box_: Box_;
};

export type Boxes = {
  boxes: BoxHash[];
};

/**
 * The volume of the box over its volume variables.
 * If the box has an excluded region, the part of that region inside the box is not counted.
 */
export function getBoxVolume(box: Box): number {
  let outlineVolume = 1;
  // the volume of the excluded region clipped to the box outline
  let exceptVolume = box.box_.except ? 1 : 0;

  // Only the volume variables count, also for the excluded region:
  // it lists the parameters too, and their domains have zero width.
  for (const varName of box.box_.volumeVars) {
    const interval = box.box_.varDomains[varName];
    if (interval === undefined) continue;
    outlineVolume *= interval.u - interval.l;

    // a variable not listed in the excluded region is not restricted by it
    const excluded = box.box_.except?.[varName] ?? interval;
    const overlapL = Math.max(interval.l, excluded.l);
    const overlapU = Math.min(interval.u, excluded.u);
    exceptVolume *= Math.max(0, overlapU - overlapL);
  }

  return outlineVolume - exceptVolume;
}

export function pickXY(box: Box): { xVar: Var; yVar: Var } {
  const vars = Object.keys(box.box_.varDomains ?? {});

  // find vars that start with 'x' and 'y'
  const xVars = vars.filter((v) => v.toLowerCase().startsWith('x'));
  const yVars = vars.filter((v) => v.toLowerCase().startsWith('y'));

  // pick xVar and yVar, preferring those that start with 'x' and 'y'
  let xVar = (xVars.length == 1 ? xVars[0] : vars[0]) ?? '_x';
  let yVar = (yVars.length == 1 ? yVars[0] : vars[1]) ?? '_y';

  // ensure xVar and yVar are different if possible
  if (xVar === yVar) {
    yVar = vars.find((v) => v !== xVar) || '_y';
  }
  return { xVar, yVar };
}
