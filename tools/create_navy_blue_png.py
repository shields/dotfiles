#!/usr/bin/env python

# Copyright © 2024-2026 Michael Shields
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

from pathlib import Path

from PIL import Image, ImageCms

img = Image.new("RGB", (100, 100), color=(2, 42, 58))

icc_bytes = bytearray(
    ImageCms.ImageCmsProfile(ImageCms.createProfile("sRGB")).tobytes(),  # pyright: ignore[reportUnknownMemberType,reportUnknownArgumentType]
)
# Zero out the date/time field (offset 24–35) in the ICC profile header
# to make the output reproducible across runs.
icc_bytes[24:36] = b"\x00" * 12

# Resolve the output relative to the repo root (this file lives in tools/) so
# the script writes the committed PNG regardless of the current directory.
output_path = (
    Path(__file__).resolve().parent.parent
    / "Library/Application Support/desktoppr/navy_blue.png"
)
img.save(output_path, icc_profile=icc_bytes)
