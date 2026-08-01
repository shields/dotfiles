#!/usr/bin/env node

// Copyright © 2025-2026 Michael Shields
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

// color-palette - Generate evenly spaced color palettes in OKLCH colorspace

import Color from "colorjs.io";

interface PaletteOptions {
  count: number;
  lightness: number;
  chroma: number;
  startHue: number;
}

interface ColorInfo {
  name: string;
  lightness: string;
  chroma: string;
  hue: string;
  srgbHex: string;
  deltaE: string;
}

const defaults: PaletteOptions = {
  count: 12,
  lightness: 0.4,
  // At lightness 0.4, 0.15 keeps the cool/magenta hues within the sRGB gamut
  // and the rest close to it; the OKLCH maximum of 0.4 puts every hue far out
  // of gamut, so each swatch would be heavily gamut-mapped and darkened.
  chroma: 0.15,
  startHue: 0,
};

function fail(message: string): never {
  console.error(`color-palette: ${message}`);
  process.exit(1);
}

function requireNumber(arg: string, value: string | undefined): number {
  const parsed = Number(value);
  if (value === undefined || value === "" || Number.isNaN(parsed)) {
    fail(`${arg} requires a numeric value`);
  }
  return parsed;
}

const args = process.argv.slice(2);
const options: PaletteOptions = { ...defaults };

for (let i = 0; i < args.length; i++) {
  const arg = args[i];
  if (arg === undefined) {
    break;
  }

  switch (arg) {
    case "--count":
    case "-c": {
      const value = requireNumber(arg, args[++i]);
      if (!Number.isInteger(value) || value < 1) {
        fail("--count requires a positive integer");
      }
      options.count = value;
      break;
    }
    case "--lightness":
    case "-l": {
      options.lightness = requireNumber(arg, args[++i]);
      break;
    }
    case "--chroma":
    case "-C": {
      options.chroma = requireNumber(arg, args[++i]);
      break;
    }
    case "--start-hue": {
      options.startHue = requireNumber(arg, args[++i]);
      break;
    }
    case "--help":
    case "-h": {
      console.log(`
Usage: color-palette [options]

Options:
  --count, -c       Number of colors to generate (default: ${String(defaults.count)})
  --lightness, -l   Lightness value (0-1, default: ${String(defaults.lightness)})
  --chroma, -C      Chroma value (0-0.4, default: ${String(defaults.chroma)})
  --start-hue       Starting hue (0-360, default: ${String(defaults.startHue)})
  --help, -h        Show this help message
    `);
      process.exit(0);
      break; // unreachable, but no-fallthrough doesn't know process.exit never returns
    }
    default: {
      fail(`unknown argument: ${arg}`);
    }
  }
}

const namedColors: Record<string, Color> = {
  black: new Color("srgb", [0, 0, 0]),
  white: new Color("srgb", [1, 1, 1]),
  red: new Color("srgb", [1, 0, 0]),
  green: new Color("srgb", [0, 1, 0]),
  blue: new Color("srgb", [0, 0, 1]),
  magenta: new Color("srgb", [1, 0, 1]),
  yellow: new Color("srgb", [1, 1, 0]),
  cyan: new Color("srgb", [0, 1, 1]),
};

// OKLCH hue angle of each chromatic anchor. We name a swatch by its nearest
// hue rather than by full-color distance: at low chroma every dark swatch is
// "closest" to black under any deltaE metric, which collapses every name onto
// Black regardless of the actual hue.
const namedHues: [string, number][] = Object.entries(namedColors)
  .filter(([name]) => name !== "black" && name !== "white")
  .map(([name, color]): [string, number] => [
    name,
    color.to("oklch").coords[2] ?? 0,
  ]);

function closestName(oklch: Color): string {
  const [lightness, chroma, hue] = oklch.coords;
  // An (almost) achromatic color has no meaningful hue; name it by lightness.
  if ((chroma ?? 0) < 0.04) {
    return (lightness ?? 0) < 0.5 ? "black" : "white";
  }
  let best = "black";
  let minDistance = Infinity;
  for (const [name, namedHue] of namedHues) {
    // Smallest absolute angle between the two hues on the [0, 360) circle.
    const distance = Math.abs(
      (((((hue ?? 0) - namedHue) % 360) + 540) % 360) - 180,
    );
    if (distance < minDistance) {
      minDistance = distance;
      best = name;
    }
  }
  return best;
}

function generatePalette(options: PaletteOptions): ColorInfo[] {
  const { count, lightness, chroma, startHue } = options;
  const palette: ColorInfo[] = [];

  for (let i = 0; i < count; i++) {
    // Normalize into [0, 360): a negative --start-hue would otherwise show
    // (and report) negative hues like -30 instead of the equivalent 330.
    const hue = (((startHue + (i * 360) / count) % 360) + 360) % 360;
    const oklchColor = new Color("oklch", [lightness, chroma, hue]);
    const p3Color = oklchColor.to("p3");
    // colorjs.io emits "#rrggbb" only in sRGB; the P3 color is gamut-mapped
    // down to sRGB for this hex.
    const srgbHex = p3Color.toString({ format: "hex" });

    const colorName = closestName(oklchColor);

    palette.push({
      name: colorName.charAt(0).toUpperCase() + colorName.slice(1),
      lightness: oklchColor.coords[0]?.toFixed(2) ?? "0",
      chroma: oklchColor.coords[1]?.toFixed(2) ?? "0",
      hue: oklchColor.coords[2]?.toFixed(0) ?? "0",
      srgbHex,
      // Gamut-mapping error (CIEDE2000): how far the displayed sRGB hex color
      // lands from the requested OKLCH color. Comparing against the unmapped
      // p3Color would always yield ~0, since it is the same point in another
      // space.
      deltaE: Color.deltaE(oklchColor, new Color(srgbHex), "2000").toFixed(2),
    });
  }

  return palette;
}

function formatMarkdownTable(palette: ColorInfo[]): string {
  const header = "| Name | Lightness | Chroma | Hue | sRGB | ΔE |";
  const separator = "| ---- | --------: | ------: | ---: | ---- | --: |";

  const rows = palette.map((color) => {
    return `| ${color.name} | ${color.lightness} | ${color.chroma} | ${color.hue}º | \`${color.srgbHex}\` | ${color.deltaE} |`;
  });

  return [header, separator, ...rows].join("\n");
}

const palette = generatePalette(options);
console.log(formatMarkdownTable(palette));
