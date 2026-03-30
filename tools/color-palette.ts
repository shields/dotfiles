#!/usr/bin/env node

// Copyright © 2025 Michael Shields
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

// Requires colorjs.io v0.6.0-alpha.1 to support P3 hex output.
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
  p3Hex: string;
  deltaE: string;
}

const defaults: PaletteOptions = {
  count: 12,
  lightness: 0.4,
  chroma: 0.4,
  startHue: 0,
};

const args = process.argv.slice(2);
const options: PaletteOptions = { ...defaults };

for (let i = 0; i < args.length; i++) {
  const arg = args[i];

  switch (arg) {
    case "--count":
    case "-c": {
      options.count = Number.parseInt(args[++i] ?? "", 10);
      break;
    }
    case "--lightness":
    case "-l": {
      options.lightness = Number.parseFloat(args[++i] ?? "");
      break;
    }
    case "--chroma":
    case "-C": {
      options.chroma = Number.parseFloat(args[++i] ?? "");
      break;
    }
    case "--start-hue": {
      options.startHue = Number.parseFloat(args[++i] ?? "");
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
    }
  }
}

function generatePalette(options: PaletteOptions): ColorInfo[] {
  const { count, lightness, chroma, startHue } = options;
  const palette: ColorInfo[] = [];

  for (let i = 0; i < count; i++) {
    const hue = (startHue + (i * 360) / count) % 360;
    const oklchColor = new Color("oklch", [lightness, chroma, hue]);
    const p3Color = oklchColor.to("p3");

    // Determine closest named color
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

    let closestName = "Unknown";
    let minDistance = Infinity;

    for (const [name, color] of Object.entries(namedColors)) {
      const distance = Color.deltaE(p3Color, color);
      if (distance < minDistance) {
        minDistance = distance;
        closestName = name;
      }
    }

    palette.push({
      name: closestName.charAt(0).toUpperCase() + closestName.slice(1),
      lightness: oklchColor.coords[0]?.toFixed(2) ?? "0",
      chroma: oklchColor.coords[1]?.toFixed(2) ?? "0",
      hue: oklchColor.coords[2]?.toFixed(0) ?? "0",
      p3Hex: p3Color.toString({ format: "hex" }),
      deltaE: Color.deltaE(oklchColor, p3Color).toFixed(2),
    });
  }

  return palette;
}

function formatMarkdownTable(palette: ColorInfo[]): string {
  const header = "| Name | Lightness | Chroma | Hue | P3 RGB | ΔE |";
  const separator = "| ---- | --------: | ------: | ---: | ------ | --: |";

  const rows = palette.map((color) => {
    return `| ${color.name} | ${color.lightness} | ${color.chroma} | ${color.hue}º | \`${color.p3Hex}\` | ${color.deltaE} |`;
  });

  return [header, separator, ...rows].join("\n");
}

const palette = generatePalette(options);
console.log(formatMarkdownTable(palette));
