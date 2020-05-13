#!/usr/bin/python3

"""Outputs a battery status string for BetterTouchTool."""

import struct
import subprocess
import xml.etree.ElementTree as ET


def main():
    root = ET.fromstring(
        subprocess.check_output(
            ["ioreg", "-w", "0", "-r", "-c", "AppleSmartBattery", "-a"],
            encoding="utf-8",
        )
    )

    # Every field we need is an integer. Unpack all of them.
    data = {}
    for elt in root.find("./array/dict"):
        if elt.tag == "key":
            key = elt.text
        elif elt.tag == "integer":
            value = int(elt.text)
            if value >= 2 ** 63:
                value = struct.unpack("q", struct.pack("Q", value))[0]
            data[key] = value

    fields = [
        f"{data['CurrentCapacity'] * data['Voltage'] / 1e6:.1f} Wh",
        f"({data['CurrentCapacity'] / data['MaxCapacity'] * 100:.0f}%)",
    ]

    watts = data["InstantAmperage"] * data["Voltage"] / 1e6
    if abs(watts) >= 1:
        fields.append(f"{watts:+.1f} W".replace("-", "−"))

    print(" ".join(fields))


if __name__ == "__main__":
    main()
