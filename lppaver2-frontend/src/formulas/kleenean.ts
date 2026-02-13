export type Kleenean = 'CertainTrue' | 'CertainFalse' | 'TrueOrFalse';

export function kleeneanFromBoolean(b: boolean): Kleenean {
  return b ? 'CertainTrue' : 'CertainFalse';
}

export function kleeneanSwitch<T>(k: Kleenean, onTrue: T, onUndecided: T, onFalse: T): T {
  switch (k) {
    case 'CertainTrue': return onTrue;
    case 'TrueOrFalse': return onUndecided;
    case 'CertainFalse': return onFalse;
  }
}

export function kleeneanNot(k: Kleenean): Kleenean {
  switch (k) {
    case 'CertainTrue': return 'CertainFalse';
    case 'CertainFalse': return 'CertainTrue';
    case 'TrueOrFalse': return 'TrueOrFalse';
  }
}

export function kleeneanAnd(k1: Kleenean, k2: Kleenean): Kleenean {
  if (k1 === 'CertainFalse' || k2 === 'CertainFalse') {
    return 'CertainFalse';
  }
  if (k1 === 'CertainTrue' && k2 === 'CertainTrue') {
    return 'CertainTrue';
  }
  return 'TrueOrFalse';
}

export function kleeneanOr(k1: Kleenean, k2: Kleenean): Kleenean {
  if (k1 === 'CertainTrue' || k2 === 'CertainTrue') {
    return 'CertainTrue';
  }
  if (k1 === 'CertainFalse' && k2 === 'CertainFalse') {
    return 'CertainFalse';
  }
  return 'TrueOrFalse';
}
