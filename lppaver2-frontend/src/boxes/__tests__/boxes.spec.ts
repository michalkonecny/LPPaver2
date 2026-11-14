import { describe, expect, it } from 'vitest';
import { getBoxVolume, type Box, type VarDomains } from '../boxes';

function mkBox(varDomains: VarDomains, volumeVars: string[], except?: VarDomains): Box {
  return { boxHash: 'test', box_: { varDomains, splitOrder: volumeVars, volumeVars, except } };
}

describe('getBoxVolume', () => {
  const xy = { x: { l: 0, u: 4 }, y: { l: 0, u: 2 } };
  const eps = { eps: { l: 0.5, u: 0.5 } };

  it('multiplies the widths of the volume variables', () => {
    expect(getBoxVolume(mkBox(xy, ['x', 'y']))).toBe(8);
  });

  it('ignores variables that are not volume variables', () => {
    expect(getBoxVolume(mkBox({ ...xy, ...eps }, ['x', 'y']))).toBe(8);
  });

  it('subtracts the excluded region', () => {
    const except = { x: { l: 0, u: 1 }, y: { l: 0, u: 2 } };
    expect(getBoxVolume(mkBox(xy, ['x', 'y'], except))).toBe(6);
  });

  it('subtracts the excluded region when it also lists a zero-width parameter', () => {
    const except = { x: { l: 0, u: 1 }, y: { l: 0, u: 2 }, ...eps };
    expect(getBoxVolume(mkBox({ ...xy, ...eps }, ['x', 'y'], except))).toBe(6);
  });

  it('counts only the part of the excluded region that is inside the box', () => {
    const except = { x: { l: 3, u: 6 }, y: { l: -1, u: 2 } };
    expect(getBoxVolume(mkBox(xy, ['x', 'y'], except))).toBe(6);
  });
});
