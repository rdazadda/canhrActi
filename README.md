# canhrActi

Analysis of accelerometer data for physical activity, sleep, sedentary behavior and circadian rhythm research. It works with ActiGraph count files (.agd) and raw recordings (ActiGraph .gt3x, Axivity .cwa, GENEActiv .bin), and its raw pipeline gives the same results as GGIR. Developed by the Center for Alaska Native Health Research.

**[Web app](https://rdazadda-canhracti.share.connect.posit.cloud/)** &nbsp;·&nbsp;
**[Windows](https://github.com/rdazadda/canhrActi/releases/latest/download/CANHRActi-Setup.exe)** &nbsp;·&nbsp;
**[macOS (Apple silicon)](https://github.com/rdazadda/canhrActi/releases/latest/download/CANHRActi-mac-arm64.dmg)** &nbsp;·&nbsp;
**[macOS (Intel)](https://github.com/rdazadda/canhrActi/releases/latest/download/CANHRActi-mac-x64.dmg)** &nbsp;·&nbsp;
**[Linux](https://github.com/rdazadda/canhrActi/releases/latest/download/CANHRActi.AppImage)** &nbsp;·&nbsp;
**[R package](#r-package)**

## Getting started

### Web app

Open the [web app](https://rdazadda-canhracti.share.connect.posit.cloud/) in any modern browser. Nothing to install.

### Desktop app

The desktop app includes R, so nothing else is needed.

- **Windows:** download [`CANHRActi-Setup.exe`](https://github.com/rdazadda/canhrActi/releases/latest/download/CANHRActi-Setup.exe) and run it, then open CANHRActi from the Start menu. If SmartScreen shows a warning, click **More info**, then **Run anyway**.
- **macOS:** download the file for your Mac, [Apple silicon](https://github.com/rdazadda/canhrActi/releases/latest/download/CANHRActi-mac-arm64.dmg) (M1 or later) or [Intel](https://github.com/rdazadda/canhrActi/releases/latest/download/CANHRActi-mac-x64.dmg). Open the .dmg and drag CANHRActi to Applications. If macOS says the app cannot be opened, go to **System Settings > Privacy & Security** and click **Open Anyway**.
- **Linux:** download [`CANHRActi.AppImage`](https://github.com/rdazadda/canhrActi/releases/latest/download/CANHRActi.AppImage), then make it executable and run it:

  ```sh
  chmod +x CANHRActi.AppImage
  ./CANHRActi.AppImage
  ```

### R package

For scripting canhrActi from your own R session (R 4.1 or later):

```r
# install.packages("remotes")
remotes::install_github("rdazadda/canhrActi")
```

The dashboard also runs from R:

```r
canhrActi::run_dashboard()
```

## Citation

If you use canhrActi in your research, please cite it:

> Azadda, R. D., AK CEAL Team, & Rasmus, S. (2026). *canhrActi: Activity, sleep and circadian analysis of accelerometer data* (Version 0.4.0) [R package]. Center for Alaska Native Health Research, University of Alaska Fairbanks. https://github.com/rdazadda/canhrActi

```bibtex
@Manual{canhrActi,
  title        = {canhrActi: Activity, Sleep and Circadian Analysis of Accelerometer Data},
  author       = {Raymond Dacosta Azadda and {AK CEAL Team} and Stacy Rasmus},
  organization = {Center for Alaska Native Health Research, University of Alaska Fairbanks},
  year         = {2026},
  note         = {R package version 0.4.0},
  url          = {https://github.com/rdazadda/canhrActi},
}
```

## Support

- Questions: rdazadda@alaska.edu
- Bug reports and feature requests: <https://github.com/rdazadda/canhrActi/issues>

## License

Copyright (c) 2025 CANHR, University of Alaska Fairbanks. All rights reserved. See [LICENSE](LICENSE).

---

Center for Alaska Native Health Research (CANHR), University of Alaska Fairbanks
