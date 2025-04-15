#!/usr/bin/env node

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

  if (arg === "--count" || arg === "-c") {
    options.count = parseInt(args[++i], 10);
  } else if (arg === "--lightness" || arg === "-l") {
    options.lightness = parseFloat(args[++i]);
  } else if (arg === "--chroma" || arg === "-C") {
    options.chroma = parseFloat(args[++i]);
  } else if (arg === "--start-hue") {
    options.startHue = parseFloat(args[++i]);
  } else if (arg === "--help" || arg === "-h") {
    console.log(`
        Usage: color-palette [options]

Options:
  --count, -c       Number of colors to generate (default: ${defaults.count})
  --lightness, -l   Lightness value (0-1, default: ${defaults.lightness})
  --chroma, -C      Chroma value (0-0.4, default: ${defaults.chroma})
  --start-hue       Starting hue (0-360, default: ${defaults.startHue})
  --help, -h        Show this help message
    `);
    process.exit(0);
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
