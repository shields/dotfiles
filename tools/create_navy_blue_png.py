#!/usr/bin/env python

from PIL import Image, ImageCms

img = Image.new("RGB", (100, 100), color=(2, 42, 58))

icc_bytes = bytearray(
    ImageCms.ImageCmsProfile(ImageCms.createProfile("sRGB")).tobytes(),  # pyright: ignore[reportUnknownMemberType,reportUnknownArgumentType]
)
# Zero out the date/time field (offset 24–35) in the ICC profile header
# to make the output reproducible across runs.
icc_bytes[24:36] = b"\x00" * 12

img.save(
    "Library/Application Support/desktoppr/navy_blue.png",
    icc_profile=icc_bytes,
)
