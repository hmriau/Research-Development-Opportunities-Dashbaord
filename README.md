 Research Development Opportunities Dashboard

![App Screenshot](screenshot.png) <!-- Add a screenshot later -->

## Overview
A Shiny web application for tracking and scoring research funding opportunities, integrated with REDCap data. The app provides:

- Real-time dashboard of opportunity metrics
- REDCap data editing capabilities
- Collaborative opportunity scoring system
- Visualizations including radar plots and trend analysis

## Frameworks & Technologies

### Core Components
- **R/Shiny** - Web application framework
- **REDCap** - Research data management (via REDCapR)
- **Auth0** - Secure authentication

### Key R Packages
```r
library(shiny)         # Web framework
library(bslib)         # Modern UI theming
library(REDCapR)       # REDCap integration
library(tidyverse)     # Data manipulation (dplyr, tidyr, purrr)
library(plotly)        # Interactive visualizations
library(echarts4r)     # Advanced radar plots
library(DT)            # Interactive tables
library(lubridate)     # Date handling
```

### UI Features
- Responsive layout with `bslib`
- Modern card-based design
- Interactive tables with filtering
- Real-time score tracking

## Installation & Setup

### Prerequisites
- R (≥ 4.0.0)
- Shiny Server or RStudio Connect

### Configuration
1. Set environment variables:
   ```bash
   REDCAP_URL=your_redcap_url
   REDCAP_TOKEN=your_api_token
   AUTH0_KEY=your_auth0_key
   ```

## Wishlist & Roadmap

### High Priority
- [ ] Data validation checks on the data editor
- [ ] Add user-specific score persistence (database backend)
- [ ] Implement scoring consensus visualization
- [ ] Export full scoring reports (PDF/Word)
- [ ] Automated email notifications for new opportunities

### Medium Priority
- [ ] Multi-select filtering for opportunity table
- [ ] Historical trend comparisons
- [ ] Integration with institutional calendars
- [ ] Dark mode toggle

### Technical Debt
- [ ] Refactor scoring data storage
- [ ] Improve error handling for REDCap outages
- [ ] Unit test coverage
- [ ] Docker deployment setup

## Known Issues
- Radar plot occasionally shows loading state when data exists (#42)
- Slider inputs sometimes reset on tab switch (#57)
- Mobile view needs optimization for scoring interface (#89)

## Contributing
Pull requests welcome! Please:
1. Open an issue first for major changes
2. Maintain consistent coding style
3. Add tests for new features

## License
[MIT](LICENSE)
