# roden.works fact-check ledger (October 2026)

Every factual claim on the site was checked against public sources. Fixes are live on branch `claude/build-roden-portfolio-vOqzM`. Each correction has its source URL in a code comment next to the change.

| Verdict | Count |
|---|---|
| VERIFIED | 76 |
| CORRECTED | 62 |
| SOFTENED | 36 |
| REMOVED | 24 |
| ATTRIBUTION-FIXED | 13 |
| OWNER-CONFIRM | ~50 |

## Questions for Evan (answers let removed items come back)

**Awards and recognition**
1. **Resolved (Oct 2026):** Evan took part as a Tulane student, and most of the People First team were Loyola students. He doesn't recognize the NOLA East plan figures (5 MW, 1,200+ jobs, 12,000 tCO2e, 8.5-mile BRT, 30% affordable, etc.), so all of them are removed. The layer details are now qualitative.
2. Were "Best Debater & Speaker" and "Best Student of 2022" real awards? If so, who gave them? (removed)
3. Boy of the Year: which Boys & Girls Club, and what year?
4. Latin honor society: what is its exact name, and what year were you inducted?

**YCOD**
5. **Resolved (Oct 2026):** Founded in 2016, when Evan was about fifteen. The site now says 2016 / "at fifteen". Note: ycod.org/about still says 2017.
6. **Resolved (Oct 2026):** IRS notice CP 575 E (Feb 3, 2022) assigned an EIN to the Youth Coalition for Organ Donation as a non-profit organization. The site now says "registered nonprofit". An EIN alone does not grant 501(c) tax-exempt status, so the site makes no 501(c)(4) claim. If Form 1024-A was filed and approved, send the determination letter and the label can go back.
7. Is your 2021 draft the text that became Senate bill S4334? Was there a 2021–22 Assembly bill number?
8. **Partly resolved (Oct 2026):** ycod.org/coverage shows that the Yahoo News, Yahoo Finance, Business Insider/Markets Insider, MarketWatch, MSN and Morningstar items were syndications of the YCOD's own PR Newswire release ("Youth Coalition For Organ Donation Strives to Save Lives"), not independent coverage. They stay off the press list. CBC is listed there as "Radio One / CBC", with no link. Which show was it, and when did it air?
9. Can you confirm the WaitList Zero, ONE8FIFTY and Chris Klug Foundation partnerships? Did you lead the group for seven or more years? Is it still active? What did your Living Donor Support Act advocacy involve?

**Policy work**
10. Our Climate: are the fellowship dates right? Did you lead the virtual town halls or help with them? Did your cohort work on Oregon's EO 20-04?
11. Partnership for Public Service: what was your actual program and title? Did you facilitate the SAMHSA focus groups, or transcribe and analyze them?
12. TABI: is the proposal written down anywhere? If the coverage and speed numbers came from your own survey, a citation would let the charts come back.
13. Is the Midtown Metairie proposal still in progress?

**Engineering**
14. Is your exact ENFRA title "Sustainability Engineer II / Asset Manager"? Does your plant scope cover only UMMC and St. Mary's?
15. Odoo: which month hit 160% of goal, and was the goal a monthly non-recurring revenue target? Did you run implementations and migrations yourself, or hand them off to consultants?
16. Convergint: was the bootcamp at the Schaumburg HQ? Did you use RSMeans?
17. ~~VA~~ **Resolved (Oct 2026):** Evan confirmed a VA co-op as a WOC employee while at Tulane. The project built tools for veterans with double-arm loss to place and remove dentures and other oral appliances on their own. The Taylor Foundation reference stays removed. The page is retitled "VA Assistive Devices", and the denture-anatomy callouts are removed from the 3D viewer, because the models are the tools, not dentures. Evan also confirmed he doesn't recall any Taylor Foundation connection.
18. Weatherhead (2023–25): what was your title, and what was the "research device compliance study"?
19. SWIS: is the claim that no long-term health study exists based on your own literature search? Does the HAPS role description (monitor deployment, datasets) match what you did?
20. Is the B.S.E. in Biomedical Engineering, 2020–2024, exactly what the diploma says?

**Studio**
21. Did you shoot on the Pocket Cinema Camera 6K? What is Moten's company actually called, and what was your role?
22. Is there a link to the Children's Museum ad? (The linked video was a jazz background video, so it was relabeled.)
23. Plato's Cave: were you the director of photography? Who narrated it? Did you use anamorphic lenses?
24. Aurora Theatre and Buffalo Central Terminal: what did you shoot, and when? Should they get a section or come off the grid?
25. ~~Vogue Italia 2020~~ **Resolved (Oct 2026):** Evan confirmed that Bizar Audi (Austin Stoll, from Orchard Park, NY; based in NYC) presented Schooltime in Buffalo in 2020, that Vogue Italy featured it in its first COVID-19 issue, and that Evan walked the runway and modeled in the editorial. The site is updated.
26. Glass: is your schedule close to Bullseye's 6mm full fuse? Is the series called "Fractured Futures"? Is "FlowIt" the right name for your 3D-printing tool?
27. Do you shoot medium format, and with which camera?

---

## Section: home


| Page/file | Claim (short) | Verdict | Action | Source |
|---|---|---|---|---|
| BentoGrid.tsx, PressSection.tsx, advocacy-and-civic/page.tsx | NOLA East plan "won a C40 Reinventing Cities Award", issued by "Mayor of New Orleans" | ATTRIBUTION-FIXED | Now: honorable mention in C40's Students Reinventing Cities (2023), with the People First team. Issuer is C40 Cities. Mayor Cantrell honored the team afterward. Index card label changed from "C40 Award" to "C40 Honorable Mention" | https://css.loyno.edu/news/sep-14-2023_loyola-team-wins-honorable-mention-global-students-reinventing-cities-competition ; https://www.c40reinventingcities.org/en/events/mayor-of-new-orleans-meets-honourable-mention-team-people-first-new-orleans-students-reinventing-cities-1820.html |
| PressSection.tsx | Media strip "covered by CBC, Yahoo News, Business Insider, WKBW, Spectrum News" | REMOVED (partial) | Kept WKBW and Spectrum News, which are both verified. Removed CBC, Yahoo News and Business Insider because no coverage could be found (3 searches) | https://www.tmj4.com/news/national/college-freshmen-in-new-york-develop-plan-to-encourage-more-organ-donors (WKBW byline, Scripps-syndicated) ; https://spectrumlocalnews.com/nys/buffalo/news/2021/01/13/college-students-push-for-more-organ-donations-in-ny- |
| AboutTeaser.tsx, FeaturedWork.tsx, advocacy-and-civic/page.tsx | YCOD co-founded "at seventeen" / "at 17" | SOFTENED | Now "in high school" to match the about and TEDx pages | https://mynews13.com/fl/orlando/news/2021/09/24/wny-teens-nominated-for-american-red-cross-award-for-organ-donation-coalition |
| FeaturedWork.tsx | NY "the lowest registration rate in the country" (present tense) | CORRECTED | Now "what was then the state with the lowest registration rate". True as of 2018, and the rate has risen since | https://nyulangone.org/news/transplant-institute-study-aims-boost-organ-donation |
| FeaturedWork.tsx | "$354.6M in guaranteed savings and a 52.5% cut in purchased electricity" | CORRECTED | ENFRA guarantees 34.4% savings, which equals "more than $354.6M in total avoided costs". The 52.5% is an expected figure. Teaser reworded to say so, matching the revised ENFRA hero | https://enfrasolutions.com/enfra-and-rochester-regional-health-launch-30-year-energy-as-a-service-partnership-to-modernize-system-wide-infrastructure-and-advance-sustainability |
| lib/constants.ts (IMPACT_STATS) | "$143.8M EaaS Partnership Value" and "$354.6M 30-Year Guaranteed Savings" shown as Evan's "By the Numbers" | ATTRIBUTION-FIXED | These are the partnership's numbers, not Evan's results. Labels are now "EaaS Partnership I Work Under" and "30-Year Avoided Costs (Partnership)". Figures verified | ENFRA link above |
| AboutTeaser.tsx, BentoGrid.tsx | Manages central energy plants "for Rochester Regional Health" (implies the whole system) | SOFTENED | Now "at two Rochester Regional Health hospitals" (UMMC and St. Mary's). The partnership covers nine | ENFRA link above; enfra/EnfraOverview.tsx |
| BentoGrid.tsx | "Earlier work covered fire and life safety integration at Convergint and ERP implementations at Odoo" | SOFTENED | Now "Earlier account executive roles covered fire and life safety integration at Convergint and ERP software at Odoo", which matches the titles on the Timeline and about pages | internal |
| PressSection.tsx, AboutTeaser.tsx | TEDxTulane talk on youth political participation | VERIFIED | Added the real title "The Myth of the Apolitical Youth" and the year (2022). Card detail was "how young people are structurally shut out of politics", which contradicts the official description, so it now says the talk argues young people are more politically engaged than they get credit for | https://www.ted.com/talks/evan_roden_the_myth_of_the_apolitical_youth |
| PressSection.tsx | Real Heroes nominee, American Red Cross, 2021 | VERIFIED | Detail now says he was nominated with three fellow co-founders, not alone | mynews13 link above |
| BentoGrid.tsx, PressSection.tsx, StudioGallery.tsx | "Runway modeling for Vogue Italy" / "Vogue Italy's feature" | SOFTENED | Now "runway modeling for BizarrAudi's SchoolTime collection, which Vogue Italy covered", matching studio/modeling. No public record found (2 searches) | none found |
| ProjectNav.tsx | SWIS "Longitudinal study on saltwater intrusion" | SOFTENED | Now "Proposed longitudinal study". The detail page shows it is a proposal | research/swis/page.tsx |
| ProjectNav.tsx | Wimley lab "peptide assemblies interacting with membrane proteins" | CORRECTED | Now "lipid membranes", matching the detail page, where the peptides act on lipid bilayers | research/wimley-lab/page.tsx |
| advocacy-and-civic/page.tsx | SAMHSA engagement about 37 to 74 | VERIFIED | Reworded to make clear this is the agency's score over the Partnership's broader collaboration. Source comment added | https://ourpublicservice.org/about/history-and-impact/samhsa-strong-teaming-up-to-transform-the-workplace |
| advocacy-and-civic/page.tsx | Metairie is "Louisiana's most populous unincorporated community" | VERIFIED | Source comment added (CDP population 143,507 in 2020, the largest CDP in Louisiana) | https://en.wikipedia.org/wiki/Metairie,_Louisiana ; https://en.wikipedia.org/wiki/List_of_census-designated_places_in_Louisiana |
| BentoGrid.tsx | "100K+ on the waiting list" | VERIFIED | Source comment added (103,223 on the list) | https://www.organdonor.gov/learn/organ-donation-statistics |
| FeaturedWork.tsx, ProjectNav.tsx, BentoGrid.tsx | $143.8M, 30-year EaaS partnership | VERIFIED | Source comment added | ENFRA link above |
| advocacy-and-civic/page.tsx | YCOD "Founded 2017" | VERIFIED | none | mynews13 link above |
| constants.ts / Hero / Footer / og route / JsonLd | "Sustainability engineer at ENFRA managing hospital energy plants..." and jobTitle "Sustainability Engineer II / Asset Manager" | VERIFIED (internal) | Consistent everywhere. Title belongs to Evan (see OWNER-CONFIRM) | internal |
| ImpactStats / Timeline | "3 Research Labs at Tulane" | VERIFIED (internal) | Matches the research page's three labs (VA prosthetics, Weatherhead/Rabito, Wimley) | research/page.tsx |
| Timeline.tsx, CaseStudyGrid.tsx (rendered on the engineering index) | B.E. degree, "Taylor Foundation", Wimley "membrane proteins" | NOTE | The engineering agent is editing these files at the same time and has already fixed B.S.E., Taylor Foundation and the two-hospital wording. I left them alone. **The CaseStudyGrid Wimley teaser still says "membrane proteins"**, which should be "lipid membranes" to match the detail page | n/a |
| StudioGallery.tsx, FeaturedWork.tsx | Camera "BlackMagic 6K" | CORRECTED | Now "Blackmagic Pocket Cinema Camera 6K", matching the studio fact-check | studio/cinematography/page.tsx (cined.com source there) |
| StudioGallery.tsx | "Documentary Work": "Narrative and documentary films" | CORRECTED | The cinematography page has no documentary (The Bridge is a poetic short, Plato's Cave a narrated short). Card is now "Short Films" | https://vimeo.com/491626637 ; studio/cinematography/page.tsx |
| PressSection.tsx, StudioGallery.tsx | Cards titled "Vogue Italy" / "Vogue Italy — BizarrAudi" (read as modeling for Vogue) | SOFTENED | Retitled "SchoolTime runway" (BizarrAudi · 2020) and "BizarrAudi SchoolTime". Vogue coverage is still mentioned in the detail line, pending confirmation | none found |
| Home / studio index | Client work for "museums, universities, and cultural institutions across Louisiana"; Moten "20+ years"; glass "COE 90" | VERIFIED (not echoed) | None of these appear in my files | n/a |
| ImpactStats (constants.ts) | "7+ Years Leading The YCOD" | OWNER-CONFIRM | Kept | none |
| ImpactStats (constants.ts), ProjectNav.tsx | 160% of non-recurring revenue goal (monthly best) at Odoo | OWNER-CONFIRM | Kept | none |
| PressSection.tsx | CBC, Yahoo News, Business Insider coverage of YCOD | OWNER-CONFIRM | Removed from the home strip pending links | none found |
| StudioGallery.tsx | Aurora Theatre and Buffalo Central Terminal cinematography cards | OWNER-CONFIRM | Kept. Neither appears on any detail page, and both cards link to /studio/cinematography, which never mentions them | none |
| StudioGallery.tsx, BentoGrid.tsx | Tulane Freeman School marketing video | VERIFIED (internal) | The cinematography page now has a Freeman School section | studio/cinematography/page.tsx |
| StudioGallery / PressSection / BentoGrid | Vogue Italy coverage of BizarrAudi SchoolTime (2020) | OWNER-CONFIRM | Softened | none |
| FeaturedWork / StudioGallery | Camera operator and editor under Albert J. Moten, Jr. (BlackMagic 6K, Sony a7s II) | OWNER-CONFIRM | Kept, consistent with the cinematography page | none |

## OWNER-CONFIRM questions for Evan
1. Did you lead the YCOD for 7+ years (2017 to 2024 or later)? The home "By the Numbers" strip says "7+ Years".
2. Odoo: was 160% your best single month against your non-recurring revenue goal? Is there anything that documents it?
3. Can you send links to the CBC, Yahoo News and Business Insider coverage of the YCOD? I could only find WKBW (Olivia Proia, syndicated across Scripps stations) and Spectrum News, so the home strip now shows only those two.
4. Aurora Theatre and Buffalo Central Terminal: what did you shoot for each, and when? Neither is described anywhere else on the site. Should they get a section on the cinematography page or come off the Studio grid?
5. Vogue Italy: can you share the link or issue where Vogue Italia covered BizarrAudi's SchoolTime (2020)? Was it in the magazine, on vogue.it, or on PhotoVogue?
6. Is "Sustainability Engineer II / Asset Manager" at ENFRA your exact current title? It appears in the JSON-LD and on the Timeline.

## Section: about


| Page/file | Claim (short) | Verdict | Action | Source |
|---|---|---|---|---|
| Awards.tsx | "C40 Reinventing Cities Award" from "Mayor of New Orleans" | ATTRIBUTION-FIXED | Evan's team "People First" (6 Loyola students + 1 Tulane) won an honorable mention in C40's Students Reinventing Cities (2023); the New Orleans winner was Imperial College's "ReNew Orleans". Mayor Cantrell honored the team, she did not issue the award. Card now reads "Students Reinventing Cities, Honorable Mention / C40 Cities · 2023" | https://css.loyno.edu/news/sep-14-2023_loyola-team-wins-honorable-mention-global-students-reinventing-cities-competition ; https://www.c40reinventingcities.org/en/events/new-orleans-winning-team-present-their-project-to-mayor-latoya-cantrell-1828.html |
| Awards.tsx | "American Real Heroes Award" (title implies win) | CORRECTED | Now "Real Heroes Education Award Nominee, 2021", nominated with three fellow co-founders | https://mynews13.com/fl/orlando/news/2021/09/24/wny-teens-nominated-for-american-red-cross-award-for-organ-donation-coalition |
| Awards.tsx | "Best Debater & Speaker" (org "Academic Competition") | REMOVED | Placeholder issuer, nothing specific to verify | none |
| Awards.tsx | "Best Student of 2022" (org "Academic Achievement") | REMOVED | Placeholder issuer, nothing specific to verify | none |
| Awards.tsx | Boy of the Year, "National youth award", "Boys & Girls Club of America" | SOFTENED | Removed "national" (BGCA's national award is "Youth of the Year"; nothing found for Evan); org now "Boys & Girls Club" | search, no result |
| Awards.tsx | Latin Honor Society "for work in Classical and Ecclesiastical Latin" | SOFTENED | Now "Honored for achievement in Latin" | none found |
| Bio.tsx | "At seventeen" co-founded YCOD | SOFTENED | Now "In high school" (news: founded by East Aurora HS Donate Life club members; they were college freshmen in 2020-21, which makes 17 in Aug 2017 unlikely) | https://www.tmj4.com/news/national/college-freshmen-in-new-york-develop-plan-to-encourage-more-organ-donors |
| Bio.tsx | YCOD is a "501(c)(4) nonpartisan lobbying organization" | SOFTENED | No IRS/ProPublica record found; now "nonpartisan advocacy group" | ProPublica search, no result |
| Bio.tsx | YCOD "went on to shape legislation" | SOFTENED | Now: its opt-out proposal became a bill in the NY Assembly (DiPietro sponsored after Sept 2018 presentation; still awaiting a vote in 2021) | TMJ4 link above; mynews13 link above |
| Bio.tsx | NY donor registration rate "the lowest in the nation" | CORRECTED | Now "then the lowest" (true as of 2018; rate has since risen) | https://nyulangone.org/news/transplant-institute-study-aims-boost-organ-donation |
| Bio.tsx | Air pollution work "contributed to published findings" on black carbon and BP | ATTRIBUTION-FIXED | The paper (Rabito et al., Indoor Air 2020; data from 2016) predates Evan's time at Tulane and doesn't list him. Now: his work "built on an earlier Tulane study" | https://pmc.ncbi.nlm.nih.gov/articles/PMC7985991/ |
| Bio.tsx, Education.tsx | "Bachelor of Engineering, Biomedical/Medical Engineering" | CORRECTED | Tulane awards the B.S.E. in Biomedical Engineering | https://catalog.tulane.edu/science-engineering/biomedical-engineering/biomedical-engineering-major/ |
| Bio.tsx | Convergint role "systems integration specialist" | CORRECTED | Now "account executive", matching the Convergint page and the brief | internal (convergint/page.tsx) |
| Bio.tsx | Manages central energy plants "for Rochester Regional Health" | SOFTENED | Now "at two Rochester Regional Health hospitals" (the partnership covers 9 locations; enfra pages say UMMC and St. Mary's) | internal (enfra/EnfraOverview.tsx) |
| Bio.tsx | $143.8M, 30-year EaaS partnership | VERIFIED | Source comment added | https://enfrasolutions.com/enfra-and-rochester-regional-health-launch-30-year-energy-as-a-service-partnership-to-modernize-system-wide-infrastructure-and-advance-sustainability |
| ted/page.tsx | Talk exists: TEDxTulane, youth political participation | VERIFIED | Added real title "The Myth of the Apolitical Youth" and date (March 2022) to heading, intro and embed subtitle. YouTube ID Bq3Swc8q0CY confirmed as this talk (TEDx Talks channel) | https://www.ted.com/talks/evan_roden_the_myth_of_the_apolitical_youth ; YouTube oEmbed |
| ted/page.tsx | YCOD influenced legislation "in two states" (x4) | CORRECTED | Every source mentions only New York. Changed to New York / NY Assembly bill | mynews13, TMJ4 links above |
| ted/page.tsx | "2 States" stat card | REMOVED | Replaced with "3,000+ coalition members by 2021" (sourced) | mynews13 link above |
| ted/page.tsx | Under-30s hold "less than 2%" of legislative seats | CORRECTED | IPU: 2.8% of MPs are 30 or under. Now "fewer than 3% ... according to the Inter-Parliamentary Union" | https://www.ipu.org/news/press-releases/2026-04/youth-representation-in-parliament-flatlines-first-time-in-12-years |
| ted/page.tsx | 50% of global population under 30 | VERIFIED | IPU source comment | IPU link above |
| ted/page.tsx | Co-founded YCOD "at 17" | SOFTENED | Now "in high school" | TMJ4 link above |
| Languages.tsx | Interlingua "the most widely used naturalistic" IAL | SOFTENED | Superlative removed. Kept IALA (1937-1951) and readable without prior study by people who know a Romance language | https://en.wikipedia.org/wiki/Interlingua |
| Skills.tsx | R "across 3 research labs" | SOFTENED | Now "for research projects" (R use in the VA prosthetics lab is unsupported) | none |
| Skills.tsx | TEDxTulane speaker | VERIFIED | none needed | TED link above |
| page.tsx (meta) | Sustainability engineer at ENFRA, Tulane BME grad, YCOD co-founder, TEDx speaker | VERIFIED | none (TEDx and YCOD confirmed; ENFRA title consistent across site) | as above |
| Bio.tsx | Odoo account executive, 160% of non-recurring revenue goal in one month | OWNER-CONFIRM | Kept (matches other pages) | none |
| Education.tsx | Tulane 2020-2024 | OWNER-CONFIRM | Kept. News calls him a college freshman in Jan 2021, which fits a 2020 start | TMJ4 link above |
| Awards.tsx | Boy of the Year | OWNER-CONFIRM | Kept, softened | none |
| Awards.tsx | National Latin Honor Society | OWNER-CONFIRM | Kept, softened | none |
| Bio.tsx | YCOD tax status | OWNER-CONFIRM | Softened | none |
| Bio.tsx | YCOD founding age | OWNER-CONFIRM | Softened | none |
| ted/page.tsx | "Led it for more than seven years" | OWNER-CONFIRM | Kept (matches YCOD page) | none |
| ted/page.tsx | Quotes/"main points" | OWNER-CONFIRM | Kept, edited only for New York | none |
| Bio.tsx / Skills.tsx / Education.tsx | Freeman School marketing content; FlowIt 3D printing; grant writing; TRIZ Associate and other certifications; Latin "Full Professional" level | OWNER-CONFIRM | Kept | none |

## OWNER-CONFIRM questions for Evan
1. Odoo: did you hit 160% of your monthly non-recurring revenue goal, and is Account Executive (Feb 2024 to Feb 2025) right?
2. Tulane: is "B.S.E., Biomedical Engineering, 2020 to 2024" exactly what is on your diploma? The site said "Bachelor of Engineering", but Tulane's degree is the B.S.E.
3. Boy of the Year: which Boys & Girls Club gave it, and in what year? Was it the local "Youth of the Year" program?
4. National Latin Honor Society: what is the exact name of the society (for example the ACL/NJCL Latin honor society or Societas Honoraria Latina), and what year were you inducted?
5. Is the YCOD registered with the IRS as a 501(c)(4)? If so, please send the EIN or the determination letter so the label can go back in.
6. How old were you when the YCOD was founded (Aug 2017)? The site said 17. News coverage suggests you were a college freshman in 2020-21, which would put you at about 15.
7. Were there any real "Best Debater & Speaker" or "Best Student of 2022" awards? If so, who gave them? Both have been removed for now.
8. Did you actually lead the YCOD for seven or more years (2017 to 2024 or later)?
9. Do the TEDx page's "main points" match what you said in the talk? The official description says the talk argues that young people are more participatory than older Americans.
10. Please confirm these items: the Freeman School marketing work, "FlowIt" as the 3D-printing tool (could it be a typo for another slicer?), grant writing for research proposals, the TRIZ Associate certification, and your Latin proficiency level.

## Section: engineering


Files: app/engineering-and-sustainability/{Timeline,SkillsRadar,CaseStudyGrid}.tsx, research/page.tsx (index), enfra/*, convergint/*, odoo/*. tsc clean.

| Page/file | Claim (short) | Verdict | Action | Source |
|---|---|---|---|---|
| enfra/EnfraHero, page.tsx | $143.8M, 30-year EaaS, announced Jan 20, 2026 | VERIFIED | Kept, source in comment | https://enfrasolutions.com/enfra-and-rochester-regional-health-launch-30-year-energy-as-a-service-partnership-to-modernize-system-wide-infrastructure-and-advance-sustainability |
| enfra/EnfraHero | Partnership is for "the hospital energy plants that produce steam, chilled water, and electricity" | CORRECTED | Scope is all nine RRH hospital locations: heating/cooling, air handling, solar, EV charging | same ENFRA release |
| enfra/EnfraHero, page.tsx metadata, SavingsVisualization | "$354.6M guaranteed savings" / "30-Year Savings" | CORRECTED | Release says 34.4% guaranteed savings "equivalent to more than $354.6M in total avoided costs". Relabeled "30-Year Avoided Costs" | same |
| enfra/EnfraHero, SavingsVisualization | $6.9M Year 1 savings presented as guaranteed | SOFTENED | Release says "projected". Label now "Year 1 Savings (Projected)" | same |
| enfra/EnfraHero | 52.5% electricity reduction | SOFTENED | It is an *expected* cut in *purchased* electricity. Relabeled | same |
| enfra/SavingsVisualization | Chart subtitle "both hospitals combined" | CORRECTED | The announced figures cover all nine RRH hospital locations, not just UMMC and St. Mary's | same |
| enfra/SavingsVisualization | Modeled $14.2M baseline / $7.3M optimized, 1.6% escalation | CORRECTED | The $14.2M baseline had no source. Recalibrated to the announced figures only: $6.9M yr-1, 34.4% guarantee, $354.6M total. Baseline is 6.9/0.344 ≈ $20.1M with one shared escalation (~3.5%/yr). Note now names the source and states the assumptions | same |
| enfra/EnfraOverview | "Founded in 1919 as Bernhard" | CORRECTED | ENFRA says "Established 1919". The Bernhard name came from five companies uniting in 2014-15. Now "traces its roots to 1919 and operated as Bernhard until May 2025" | https://enfrasolutions.com/about ; https://enfrasolutions.com/bernhard-rebrands-as-enfra-to-reflect-energy-infrastructure-leadership-and-future-growth-2 |
| enfra/EnfraOverview | 2,600+ employees, 25+ offices | CORRECTED | ENFRA about page: 3,000+ employees, 29 office locations | https://enfrasolutions.com/about |
| enfra/EnfraOverview | 24 states; rebrand May 2025 | VERIFIED | Kept | rebrand release above |
| enfra/EnfraOverview | $2B+ financed projects, $87M guaranteed annual savings | VERIFIED | Kept | https://enfrasolutions.com/about |
| enfra/EnfraOverview | EaaS >35% CAGR since 2017 | VERIFIED | Kept | rebrand release above |
| enfra/EnfraOverview | Ochsner "first not-for-profit EaaS deal, 2017" | CORRECTED | Source says it was the first U.S. healthcare Energy Asset Concession, 2017 | https://bernhard.com/?p=7574 |
| enfra/EnfraOverview | Hackensack Meridian $134M | VERIFIED | Kept | https://informedinfrastructure.com/post/bernhard-and-hackensack-meridian-health-forge-a-transformative-30-year-energy-partnership |
| enfra/EnfraOverview | Beacon Health $54.2M | VERIFIED | Kept | https://enfrasolutions.com/enfra-and-beacon-health-system-partner-on-30-year-energy-as-a-service-agreement-to-advance-sustainability-and-efficiency-2 |
| enfra/EnfraOverview | Novant $855M "largest healthcare EaaS transaction to date" | SOFTENED | Now attributed to ENFRA and dated to the 2025 announcement | https://enfrasolutions.com/projects/novant-health |
| enfra/EnfraOverview, CaseStudyGrid | "St. Mary's Medical Center" | CORRECTED | RRH calls it "St. Mary's Medical Campus" | https://rochesterregional.org/locations/medical-campuses/st-marys |
| enfra/EnfraOverview | RRH: nine hospitals, 19,400+ employees, second-largest employer in Rochester | VERIFIED | Kept | https://www.rochesterregional.org/about/facts-and-statistics |
| enfra/EnfraOverview | RRH "500+ ambulatory facilities" | CORRECTED | RRH says 557 practice locations | same |
| enfra/EnfraOverview | "Goal: 100% renewable electricity" | SOFTENED | RRH's stated goal was 100% of its electricity by 2025. Date added | https://rochesterregional.org/hub/solar-energy |
| enfra/EnfraOverview | "Second-largest solar project in NYS (5.5 MW)" | CORRECTED | It is 5.48 MW in Parma, NY, the second-largest *single-site* solar farm in NYS *when it came online in 2019* | https://greensparksolar.com/2019/04/24/rochester-regional-health/ |
| enfra/FacilityMap | UMMC 131 beds; largest private employer in Genesee County; sole maternity provider | VERIFIED | Kept. Named the counties (Genesee and Orleans) | https://en.wikipedia.org/wiki/United_Memorial_Medical_Center ; https://rochesterregional.org/locations/hospitals/batavia |
| enfra/FacilityMap | UMMC 785+ employees | CORRECTED | 785 is Wikipedia's older count; RRH's page now says more than 900 | https://rochesterregional.org/locations/hospitals/batavia |
| enfra/FacilityMap | St. Mary's opened 1857 | VERIFIED | Kept | https://rbj.net/2007/09/13/hospital-to-mark-sesquicentennial/ |
| enfra/FacilityMap | 13,000+ dialysis treatments; behavioral health, homeless care, senior housing | VERIFIED | Kept | https://rochesterregional.org/locations/medical-campuses/st-marys |
| enfra/FacilityMap, map-data.ts | Facility coordinates, 29-mi distance, real geography | VERIFIED | UMMC coordinates match Wikipedia (43°00′18″N 78°10′36″W). Census and OSM sources already cited | Wikipedia above |
| enfra/plant-model, EnergyPlantDiagram | Equipment ratings (2×1,200-ton chillers, etc.) | VERIFIED | Already labeled visibly "Representative schematic... ratings are illustrative". NFPA 110 Type 10 = 10 s transfer is correct | NFPA 110 |
| Timeline | ENFRA: CEPs "at Rochester Regional Health" | SOFTENED | Changed to "at two Rochester Regional Health hospitals" to match the role | internal consistency |
| convergint/page (CdpJourney data), Timeline | Bootcamp in "Chicago, IL" | CORRECTED | Now "Schaumburg, IL" in both places. HQ was at 1 Commerce Dr, Schaumburg until the Oct 2025 move to Bell Works, Hoffman Estates | https://dailyherald.com/?p=1301823 ; https://www.securitysystemsnews.com/article/convergint-moves-chicago-area-operations-new-facility |
| convergint/page | $2.6B revenue, 10,000+ colleagues, 220+ locations, #1 SDM 8 yrs | VERIFIED | Kept as of July 2025, dated in labels and copy. Note: Convergint's Oct 2025 release says 11,000+ colleagues | https://www.convergint.com/press-releases/convergint-named-1-systems-integrator-by-sdm-magazine-for-eighth-year-in-a-row/ |
| convergint/page | Certified service partner for Notifier, Edwards, Simplex, Siemens | SOFTENED | Kept Edwards ("one of the largest Edwards partners in the world") and Honeywell/Silent Knight. Removed Notifier, Simplex and Siemens as unverified | https://www.convergint.com/edwards/ (indexed; 404 on recheck) ; https://old.convergint.com/?p=197304 |
| convergint/FireSystemDiagram | Sprinkler system type "selected based on hazard classification per NFPA 13" | SOFTENED | Type also depends on freezing risk and water-damage sensitivity. Reworded | NFPA 13 general |
| convergint/FireSystemDiagram | Other NFPA 72 / 2001 / UFGS behaviour | VERIFIED | Already sourced in a file header. No change | in-file sources |
| odoo/page | 13M+ users | CORRECTED | Odoo now says 28 million users (13M was the Nov 2024 figure) | https://www.odoo.com/page/about-us |
| odoo/page | €5B valuation "above €5 billion" | CORRECTED | The €5B round was Nov 2024. Odoo announced €10B on Sept 24, 2026. Both are now stated with dates; the hero stat shows €10B (2026) | https://www.brusselstimes.com/2332306/odoo-announces-e10-billion-valuation-and-a-partial-price-increase ; https://www.summitpartners.com/news/odoo-announces-a-500-million-transaction-increasing-the-belgian-unicorns-valuation-to-5-billion |
| odoo/page | "Backed by CapitalG, Sequoia, and BlackRock" | ATTRIBUTION-FIXED | CapitalG and Sequoia *led* the €500M secondary. BlackRock was one of several participants (with Mubadala, HarbourVest, AVP and Alkeon) | Summit Partners release |
| odoo/page | Founded 2005 | REMOVED | Sources conflict: Odoo's 2024 release says April 2002, Wikipedia says 2005. Kept founder and Belgium | Summit release ; https://en.wikipedia.org/wiki/Odoo |
| odoo/page | 82 official modules | CORRECTED | Odoo says "50 main applications" | https://www.odoo.com/page/about-us |
| odoo/page | 50,000+ community apps | VERIFIED | Kept | same |
| odoo/page | 180+ countries | REMOVED | Not on Odoo's official page. The one news mention was unclear | — |
| odoo/page | Analytic accounting, barcode, IoT are Enterprise-only | VERIFIED | Kept | https://www.odoo.com/page/editions |
| odoo/page JSON-LD | "manufacturing and distribution companies" | CORRECTED | Matched to the rest of the site: manufacturing, food and beverage, and retail | internal consistency |
| odoo/GoalBulletChart | Chart values | VERIFIED | Only the goal (100%) and best month (160%) are drawn. No other months were invented | — |
| CaseStudyGrid | "Hit 160% of non-recurring revenue goal" (implies overall) | SOFTENED | Added "in my best month" to match the Odoo page | internal consistency |
| research/page | HAPS "Published finding linking highest-quartile black carbon... SBP" | ATTRIBUTION-FIXED | The published result is Rabito et al., Indoor Air (+7.55 mmHg per 1 µg/m³). It used earlier data and does not list Evan. Now "building on an earlier Tulane study" | https://pmc.ncbi.nlm.nih.gov/articles/PMC7985991/ |
| research/page | 2023 crisis threatened water for "1.2 million residents" | CORRECTED | PBS/AP: "close to a million residents in four parishes". NOTE: research/swis/page.tsx (not my file) still says 1.2M in three places | https://www.pbs.org/newshour/nation/why-salt-water-is-threatening-drinking-water-in-new-orleans-and-what-officials-are-doing-about-it |
| research/page, Timeline, CaseStudyGrid | VA prosthetics: "Taylor Foundation partnership", limb-difference devices | SOFTENED | Removed Taylor Foundation (unverified). Now names the New Orleans VA Medical Center. Described as dental/maxillofacial prosthetic models, per the coordinator's research findings | coordinator / va-prosthetics page |
| research/page | "I spent four years doing ... research" | SOFTENED | Dates on the site run from May 2022 to Jan 2025. Now "From 2022 to 2025" | internal consistency |
| CaseStudyGrid | Wimley peptides interact with "membrane proteins" | CORRECTED | Now "lipid membranes", matching the detail page and the research index | coordinator / wimley-lab page |
| Timeline | "B.E. Biomedical/Medical Engineering" | CORRECTED | Now "B.S.E. in Biomedical Engineering", per the coordinator's about-section finding | coordinator |
| Timeline / convergint / odoo | Odoo Feb 2024–Feb 2025; Convergint Feb–Dec 2025 SF; ENFRA 2026– | VERIFIED | Consistent across Timeline, project pages and CdpJourney (Month 3-11 = Apr–Dec) | internal |
| SkillsRadar | Skills | VERIFIED | No ratings or numbers. Each skill has context lines only and is not labeled as measured. No change | — |
| Timeline | "TEDx speaker"; "Three research labs" | OWNER-CONFIRM | Evan, can you send the TEDx event name/year and a link to the talk? A search found nothing under your name. |
| Timeline | Tulane Weatherhead 2023–2025, "Biomedical Engineering Researcher", "Research device compliance study" | OWNER-CONFIRM | Evan, was your title at Weatherhead "Biomedical Engineering Researcher", and what was the "research device compliance study"? It isn't described anywhere else on the site. |
| Timeline | VA 2022–2025 "Biomedical Engineer / Project Manager" | OWNER-CONFIRM | Evan, were you a VA employee with that title, or a Tulane student working with the New Orleans VA Medical Center? Is there any Taylor Foundation connection we can cite? |
| enfra/EnfraOverview, plant-model roles | Sustainability Engineer II / Asset Manager; managing CEPs at UMMC and St. Mary's; subcontractors, budgets, energy data | OWNER-CONFIRM | Evan, please confirm your exact title and that your plant scope is just UMMC and St. Mary's (not the other seven RRH sites). |
| convergint/page | Bootcamp at HQ in Schaumburg (4 weeks), field training 4 weeks, accounts months 3-11; RSMeans; nurse call training | OWNER-CONFIRM | Evan, was the CDP bootcamp held at the Schaumburg HQ (not downtown Chicago or elsewhere)? Did you use RSMeans for estimating? |
| odoo/page, CaseStudyGrid, GoalBulletChart | 160% of monthly non-recurring revenue goal in one month | OWNER-CONFIRM | Evan, can you confirm the 160% month (which month, and was the target monthly non-recurring revenue)? |
| odoo/page, erpFlow.ts | Managed full implementation cycles; MRP for discrete manufacturers; analytic accounting "specialization"; migrations from QuickBooks/Excel/Sage; portal as common add-on | OWNER-CONFIRM | Evan, as an Account Executive did you run implementations yourself, or sell them and hand off to consultants? Did you personally do the QuickBooks/Sage migrations? |

## Section: research


Files: va-prosthetics/, haps/, swis/, wimley-lab/ (+ components/three/ProstheticViewer.tsx).

| Page/file | Claim (short) | Verdict | Action | Source |
|---|---|---|---|---|
| wimley-lab/page.tsx | ATRAM listed as a Wimley lab peptide | ATTRIBUTION-FIXED | Replaced ATRAM with MelP5 (a genuine Wimley gain-of-function melittin variant, parent of the macrolittins). ATRAM is from Francisco Barrera's lab, Univ. of Tennessee. | https://pmc.ncbi.nlm.nih.gov/articles/PMC6408977 (ATRAM/Barrera, UTK); https://medicine.tulane.edu/wimley-lab/pore-forming-peptides (MelP5/macrolittins) |
| wimley-lab/page.tsx | "$1.6M NIH grant for nanopore medicine research" | REMOVED | Deleted bullet; no source found on Tulane/Wimley pages or in search. | n/a (unsourced) |
| wimley-lab/page.tsx | "Combinatorial peptide libraries with 10,000+ variants per screen" | REMOVED/SOFTENED | Removed the specific unverified count; reworded to "screened by iterative synthetic molecular evolution." | n/a (specific number unsourced) |
| wimley-lab/page.tsx | "Collaboration with Tulane Biochemistry and Biomedical Engineering departments" | CORRECTED | Changed to the documented collaboration with the Hristova Lab, Johns Hopkins. | https://medicine.tulane.edu/wimley-lab/pore-forming-peptides |
| wimley-lab/page.tsx | Macrolittins "form stable pores at nanomolar concentrations and stay antibacterial under physiological conditions" | CORRECTED/SOFTENED | Reworded to match source: large pores at very low peptide:lipid ratios, release macromolecular cargo, no measurable toxicity to human cells. | https://medicine.tulane.edu/wimley-lab/pore-forming-peptides |
| wimley-lab/page.tsx | Antimicrobial peptides "stay active in whole blood" | VERIFIED | Kept. Wimley lab's antibacterial peptides function in whole blood without damaging human cells. | https://news.tulane.edu/news/pioneering-peptide-research-may-tackle-antibiotic-resistant-disease |
| wimley-lab/page.tsx | William Wimley = George A. Adrouny Professor of Biochemistry, Tulane School of Medicine | VERIFIED | Kept. | https://medicine.tulane.edu/departments/biochemistry-molecular-biology-tulane-cancer-center/faculty/william-c-wimley-phd |
| wimley-lab/page.tsx | pHD peptides form nanopores active at pH < 6, inactive at 7.4 | VERIFIED | Kept (pH-triggered macromolecule-sized pores are a documented Wimley evolved property). | https://medicine.tulane.edu/wimley-lab/synthetic-molecular-evolution-peptides |
| wimley-lab/page.tsx | No claim that Evan published / co-authored | VERIFIED | No authorship claim present; page says "Research Assistant." No Roden+Wimley publication found, so nothing to soften. | PubMed/Scholar search negative |
| wimley-lab/MembraneModel.tsx | pHD gate at pH<6, macrolittins pH-independent stable pore | VERIFIED | Labeled schematic/not-to-scale; consistent with page facts. No change. | as above |
| haps/data.ts, HapsSections.tsx | Black carbon: "highest quartile ... ~2 mmHg higher systolic BP" | CORRECTED | Replaced with the actual published finding: +7.55 mmHg systolic per 1 µg/m³ black carbon (P=.02). No quartile analysis exists in the study. Updated both the pollutant description and the Evidence hero number/label/body. | https://pmc.ncbi.nlm.nih.gov/articles/PMC7985991/ (Rabito et al. 2021) |
| haps/page.tsx | Study "led by Dr. Felicia Rabito" at Tulane | VERIFIED | Kept. Rabito is lead author of the HAPS black-carbon/BP study. | https://pmc.ncbi.nlm.nih.gov/articles/PMC7985991/ ; https://hapsnola.wp.tulane.edu/ |
| haps/HapsSections.tsx | Lewington: ~7% IHD / ~10% stroke mortality per 2 mmHg usual SBP | VERIFIED | Kept (correctly cited, real meta-analysis; honest "population associations, not deaths from this study" note retained). | Lewington et al., Lancet 2002;360:1903-13 |
| haps/data.ts | WHO AQG: PM2.5 5/15, NO2 10/25 µg/m³ (2021) | VERIFIED | Kept. Matches WHO 2021 global air quality guidelines Table 0.1; BC correctly shown as no-guideline. | https://www.who.int/publications/i/item/9789240034228 |
| haps/data.ts | Instruments: pDR-1500, MicroAeth AE51, Ogawa, ambulatory BP monitor | VERIFIED | Kept. AE51 microaethalometer is the instrument used in the Rabito study; others are standard and plausible. | https://pmc.ncbi.nlm.nih.gov/articles/PMC7985991/ |
| swis/page.tsx | "2x in 12 years (2012 and 2023)" | CORRECTED | Undercount. Emergency sills built 1988, 1999, 2012, 2022, 2023. Changed stat to "5 / sills since 1988." | https://gohsep.la.gov/about/news/usace-to-construct-underwater-sill-to-arrest-saltwater-progression-into-mississippi-river/ |
| swis/page.tsx | "0 longitudinal studies ... No long-term health study has ever tracked..." | SOFTENED | Removed the unsourceable absolute; reframed as the author's own search ("I found no...") and the gap this proposal fills. Stat changed to "New / longitudinal study." | n/a (absolute, unsourceable) |
| swis/page.tsx | 1.2M residents; 2023 flow 130k-150k cfs vs ~300k threshold; Corps sill; Carrollton intake | VERIFIED | Kept. Consistent with the sourced constants in wedgePhysics.ts. | wedgePhysics.ts source list (NBC, CSM, DVIDS, WWNO, NPS) |
| swis/wedgePhysics.ts | All physical constants (300k threshold, RM 103.7 @130k, 1988 record low 120k→Kenner, sill RM 64, Carrollton RM 104.7, depths) | VERIFIED | No change. File is meticulously sourced with inline URLs; spot-checked threshold, reference toe, sill site, intake miles — all consistent with cited USACE/NOAA/press sources. | inline SOURCES map in file |
| swis/WedgeModel.tsx / WedgeProfile.tsx | Modeled toe table + "illustrative model, not a forecast" note | VERIFIED | No change. Clearly labeled illustrative; calibration points traceable to wedgePhysics sources. | as above |
| va-prosthetics/page.tsx | Tulane–U.S. Dept. of Veterans Affairs 3D-printing prosthetics partnership | VERIFIED (partnership) | Kept. Tulane BME does collaborate with the New Orleans VA on 3D-printed prosthetics (via adjunct Brian Layman). | https://sse.tulane.edu/node/4852 |
| va-prosthetics/page.tsx | "veterans dealing with limb loss and limited mobility"; quote "gripping a coffee cup or reaching a shelf" | SOFTENED | The 3D models/annotations shown are dental/maxillofacial (palatal framework, denture base, prosthetic teeth), not limb prosthetics. Generalized the limb-specific copy to remove the mismatch. | internal consistency (ProstheticViewer.tsx annotations + va-dent-*.glb) |
| components/three/ProstheticViewer.tsx | Device annotations (palatal framework, retention clasps, denture base, occlusal surface; "from CT scan data") | VERIFIED (internally consistent) | No change. Generic, plausible dental-device descriptions; consistent with the dental models. Flagged the page-copy mismatch above. | n/a |

## OWNER-CONFIRM (for Evan)

1. **VA prosthetics — "Taylor Foundation":** The site (this page's body, plus the research index and Timeline, which are outside my scope) says the Tulane–VA prosthetics work was "under the Taylor Foundation." I could not verify a "Taylor Foundation" tied to this program; the documented Tulane–VA 3D-printing link runs through the New Orleans VA Medical Center / adjunct Brian Layman. Is "Taylor Foundation" the correct sponsor name? If not, what is it (so it can be corrected consistently across pages)?
2. **VA prosthetics — device type:** The narrative described "limb loss / limited mobility" and "gripping a coffee cup," but the 3D models and viewer annotations are dental/maxillofacial prosthetics (palatal framework, denture base, prosthetic teeth). I softened the copy to generic "everyday ability" language. What devices did you actually design in this program — dental/maxillofacial, limb/assistive, or both? (So the copy and the 3D viewer match.)
3. **SWIS — longitudinal-study gap:** I softened "No long-term health study has ever tracked..." to "I found no long-term health study tracking..." Confirm this reflects your own literature search, or supply a citation/review if you have one.
4. **HAPS — role wording:** The page states you "deployed and retrieved the PM2.5 and NO2 monitors ... and helped turn raw sensor output into datasets ready for analysis." Plausible and internally consistent; confirm this matches your actual responsibilities on the Rabito study.

## Section: advocacy


| Page/file | Claim (short) | Verdict | Action | Source |
|---|---|---|---|---|
| ycod/CrisisDashboard, YcodStory | 100,000+ on the national waiting list | VERIFIED | Kept, source in comment | https://www.organdonor.gov/learn/organ-donation-statistics |
| ycod/YcodHero, CrisisDashboard, YcodStory | 17 people die each day waiting | VERIFIED | Kept, source in comment | https://www.organdonor.gov/learn/organ-donation-statistics |
| ycod/CrisisDashboard | "Approximately 7,500 organs are wasted annually" | REMOVED | Replaced with "a new person added every 8 minutes" (HRSA) | https://www.organdonor.gov/learn/organ-donation-statistics |
| ycod/CrisisDashboard, YcodStory | NY "lowest donor designation rate, 37–42%" (present tense) | CORRECTED | Now "about 37% when we started, then the lowest in the nation; passed 50% in 2024, below 64% national avg." | https://www.wxyz.com/news/national/college-freshmen-in-new-york-develop-plan-to-encourage-more-organ-donors ; https://cityandstateny.com/opinion/2025/03/opinion-new-york-reached-major-health-milestone-we-cannot-take-our-foot-gas/403592 |
| ycod/DonorRateChart (state rates NY 37, MT 89, AK 87, etc.) | 8-state donor designation rates | REMOVED | Chart removed (no source matches these values; DLA data by state is inconsistent/missing). Replaced with sourced prose card. File deleted | Searched DLA annual updates 2013/2021/2022, Newsweek 2025 DLA analysis |
| ycod/WaitTimeChart, YcodStory | Kidney wait 1,335 days (Black) vs 734 (white) | REMOVED | Figure is a 2009 Univ. of Maryland stat comparing Black patients with all other patients, not white patients. Chart and sentence removed. File deleted | https://kffhealthnews.org/morning-breakout/dr00010843 |
| ycod/WaitTimeChart | Hispanic 1,050 / Asian 900 days; group waitlist/donor shares | REMOVED | No source; chart removed | n/a |
| ycod/YcodStory, CrisisDashboard | Black Americans 27% of waitlist, 13% of donors | CORRECTED | 27% kept; donors now "about 12%" per current OMH page (12.28%) | https://minorityhealth.hhs.gov/organ-transplants-and-blackafrican-americans |
| ycod/YcodStory, CrisisDashboard | 60% of waitlist are people of color | VERIFIED | Kept, added "40% of the population" context | https://www.organdonor.gov/sites/default/files/organ-donor/professional/materials/lets-talk-donor-diversity-infographic-english.pdf |
| ycod/page, YcodHero | YCOD is a 501(c)(4) | REMOVED | No record found; now "youth-led coalition" | Coordinator finding; no IRS/news record |
| ycod/YcodStory | "I co-founded The YCOD at seventeen" | CORRECTED | Now "in high school" (news coverage puts founders at East Aurora HS) | https://mynews13.com/fl/orlando/news/2021/09/24/wny-teens-nominated-for-american-red-cross-award-for-organ-donation-coalition |
| ycod/YcodStory, LegislativeTimeline | Co-founders Henry McLaughlin, Grace Tapani, Sage Sellers | VERIFIED | Kept | Spectrum News (above) |
| ycod/YcodStory, LegislativeTimeline | A07954 = opt-out/presumed consent at DMV | VERIFIED | Added session (2019–20), sponsor Asm. David DiPietro, introduced May 29, 2019; died in Transportation Committee | https://www.nysenate.gov/legislation/bills/2019/A7954 |
| ycod/YcodStory, LegislativeTimeline | "I drafted the 2021 revision of A07954" | CORRECTED | A07954 is a 2019–20 bill and could not be revised in 2021. Copy now names the 2021 Senate version S4334 (Sen. Gallivan, Feb 3, 2021) and keeps "I wrote the 2021 revised draft" as Evan's claim | https://www.nysenate.gov/legislation/bills/2021/S4334 |
| ycod/YcodStory, LegislativeTimeline | Living Donor Support Act passed in 2023 | CORRECTED | Signed Dec 29, 2022 (S1594/A146, Ch. 814 of 2022). Timeline moved to 2022. Reimburses lost wages, sick/vacation days, travel, lodging, child/elder care, medical costs (cap around $14,000). Program launched Oct 2025 | https://www.nysenate.gov/legislation/bills/2021/S1594 ; https://nysfocus.com/2025/10/28/new-york-organ-kidney-donation-reimbursement |
| ycod/YcodStory, LegislativeTimeline | 2021 American Red Cross Real Heroes Education Award nominee | VERIFIED | Kept | Spectrum News (above) |
| ycod/MediaWall, YcodStory, LegislativeTimeline | Coverage by CBC, Yahoo News, Business Insider | REMOVED | No such coverage found (3+ searches; home fact-check found none either). Replaced with verified WKBW, Spectrum News, WENY, Scripps syndication | see OWNER-CONFIRM |
| ycod/MediaWall | "Local Media Network" | REMOVED | Placeholder | n/a |
| ycod/MediaWall, LegislativeTimeline | WKBW and Spectrum News coverage | VERIFIED | Kept; WENY and Scripps syndication added with sources; timeline year now 2020–21 (stories ran Dec 2020 to Sept 2021) | https://www.wxyz.com/news/national/college-freshmen-in-new-york-develop-plan-to-encourage-more-organ-donors ; https://spectrumlocalnews.com/nys/buffalo/news/2021/01/13/college-students-push-for-more-organ-donations-in-ny- ; https://weny.com/story/43131791/college-activists-pushing-for-change-to-organ-donor-registration-process-in-nys |
| ycod/LegislativeTimeline | "After more than seven years leading The YCOD" | SOFTENED | Now "After about seven years with The YCOD" | Coordinator guidance |
| ycod/VideoFeature | Talk title "TEDxTulane: Youth Political Participation" | CORRECTED | Official title "The Myth of the Apolitical Youth, TEDxTulane" | YouTube oEmbed for Bq3Swc8q0CY (TEDx Talks channel) |
| our-climate/page | CLCPA credited as a fellowship-cohort win | ATTRIBUTION-FIXED | Signed July 18, 2019, before the Nov 2019 fellowship. Now presented as context with its date | https://www.aljazeera.com/amp/economy/2019/7/18/ny-governor-signs-into-law-most-ambitious-climate-plan-in-the-us ; https://www.lw.com/admin/upload/SiteAttachments/Alert%202547v2.pdf |
| our-climate/page | CLCPA: 70% renewable by 2030, net-zero by 2050 | VERIFIED | Kept | Latham & Watkins alert (above) |
| our-climate/page, StateTileMap | MA "~$500M for green energy retrofits" (hero stat + card) | REMOVED | No source for a 2019–20 MA $500M retrofit win (about $500M in climate spending came in the 2021–22 ARPA bills, after the fellowship). Replaced with MA Next-Generation Roadmap law, signed Mar 26, 2021, dated as after the fellowship. Hero stat now "45% / 2035 Cut Target (OR)" | https://daypitney.com/insights/publications/2021/03/30-massachusetts-enacts-major-climate-change-leg ; https://www.masstaxpayers.org/sites/default/files/publications/2023-02/MTF%20Session%20Preview%20-%20Climate.pdf |
| our-climate/page | Oregon governor's executive order after cap-and-trade failed | VERIFIED | Added specifics (EO 20-04, Mar 10, 2020; 45% below 1990 by 2035, 80% by 2050) and a Mar 2020 timeline entry. This is the only win inside the fellowship | https://climate-xchange.org/2020/03/republican-walkout-halts-cap-and-invest-again-but-gov-brown-commits-to-climate/ |
| our-climate/page | "Jul 2020: cohort contributed to wins in NY, MA, OR"; "3 State Victories"; "cohort contributed to legislative wins in three states" | ATTRIBUTION-FIXED | Removed the timeline item. Stat is now "3 States Covered". Overview and map copy now date each win relative to the fellowship | sources above |
| our-climate/page | Our Climate is a youth-led 501(c)(3) based in Portland | CORRECTED | Our Climate is a 501(c)(4) (with a separate 501(c)(3) Education Fund), headquartered in DC. Copy now says "a nonprofit" and that Evan was based in Portland | https://causeiq.com/organizations/our-climate,464237362 ; https://www.guidestar.org/profile/26-3059927 |
| our-climate/StateTileMap | Map data (states highlighted) | VERIFIED | Data is only the 3 states, now dated and sourced. Titles, legend and aria text no longer say "cohort victories". Tile layout is NPR's grid (already cited) | page.tsx comments |
| partnership/page | Joined "through the Future Leaders program"; Future Leaders 10–12 wk, $6,500 + $5,500 housing | ATTRIBUTION-FIXED | Future Leaders in Public Service launched with a summer 2022 cohort, after this Sept 2021 to Jan 2022 role. Its interns are placed at agencies, and the $6,500/$5,500 figures are one university track (NSF). All references and the stipend paragraph removed | https://ourpublicservice.org/know-the-facts/blog/welcoming-the-future-leaders-in-public-service ; https://news.clearancejobs.com/2021/12/16/initiative-for-400-paid-internships-in-the-federal-government-underway/ |
| partnership/page | SAMHSA administers "National Suicide Prevention Lifeline" | CORRECTED | Now "988 Suicide & Crisis Lifeline (the National Suicide Prevention Lifeline until July 2022)"; "SAMHSA Treatment Locator" is now FindTreatment.gov | https://www.samhsa.gov/find-help/988 |
| partnership/page | SAMHSA "$6.5B Annual Budget" | VERIFIED | Labeled "FY 2022 Budget" | https://acmhai.org/news/key-spending-package-signed-into-law-includes-billions-for-mental-health-and-substance-use-services/ |
| partnership/page | SAMHSA "500+ employees" | VERIFIED | Kept (527 in May 2026, about 603 in 2012) | https://usafacts.org/explainers/what-does-the-us-government-do/subagency/substance-abuse-and-mental-health-services-administration/ |
| partnership/page, EngagementDumbbell | Engagement score ~37 to 74 | VERIFIED | BPTW: 37.1 (2020) to 74.2 (2022); copy now names the years | https://bestplacestowork.org/rankings/detail/?c=HE32 ; https://ourpublicservice.org/about/history-and-impact/samhsa-strong-teaming-up-to-transform-the-workplace |
| partnership/EngagementDumbbell | Category scores (Effective Leadership 28 to 63, Employee Development 31 to 58, Communication & Trust 33 to 67, Work-Life 45 to 71) | CORRECTED | Replaced with BPTW 2020 to 2022 values (rounded): Engagement 37 to 74, Senior Leaders 29 to 74, Supervisors 72 to 85, Pay & Benefits 68 to 73. Legend now 2020/2022, note cites BPTW | https://bestplacestowork.org/rankings/detail/?c=HE32 |
| partnership/page | "The work fed into the Best Places to Work rankings" | ATTRIBUTION-FIXED | The rankings come from OPM FEVS data. Copy now says the project used the same FEVS data | https://bestplacestowork.org/rankings/detail/?c=HE32 |
| partnership/page | "Agency Leadership Program" (named program) | SOFTENED | No program by that name found; now "the Partnership's work with agency leaders" | search: ourpublicservice.org |
| partnership/page | PPS founded 2001 by Samuel J. Heyman, $25M; BPTW, Sammies, Center for Presidential Transition | VERIFIED | Kept | https://en.wikipedia.org/wiki/Partnership_for_Public_Service |
| partnership/page | SAMHSA partnered with PPS starting Aug 2021 | VERIFIED | Added to timeline | https://ourpublicservice.org/about/history-and-impact/samhsa-strong-teaming-up-to-transform-the-workplace |
| partnership/page | "I ran focus group research" | SOFTENED | Now "I worked on focus group research" (the workstream copy says he produced transcripts and analyses) | internal consistency |
| tabi/page, CoverageChart | Coverage by area incl. "Cayuga County Avg." | REMOVED | Unsourced; Aurora is in Erie County, not Cayuga. Chart removed, files deleted, replaced with sourced prose | n/a |
| tabi/page, SpeedChart | Rural Aurora 12/1.5 Mbps, urban national avg 195/24, TABI target 100/100 | REMOVED | Unsourced; chart removed and file deleted | n/a |
| tabi/page | "Nearly one in three households lack adequate internet" | REMOVED | Unsourced | n/a |
| tabi/page | Rural residents "routinely" below 25/3, many with no wired option | SOFTENED | Now only says the village is better served than the rural parts of town (see OWNER-CONFIRM) | n/a |
| tabi/page | FCC 25/3 broadband threshold | VERIFIED | Added that it applied 2015 to March 2024, when the FCC raised it to 100/20; IIJA unserved/underserved definitions | https://broadbandbreakfast.com/fcc-increases-broadband-benchmark/ ; https://benton.org/blog/how-fcc-got-10020 |
| tabi/page | ErieNet county fiber context (new) | VERIFIED | Added: about 400-mile open-access backbone, $36M ARPA | https://www.wkbw.com/news/local-news/buffalo/erienet-plans-taking-shape-400-miles-of-fiber-cable-to-be-installed-next-month |
| midtown-metairie/page | Fat City "$13M CDBG" redevelopment (streetscaping, facades) | CORRECTED | Now Fat City Leisure Park: $17M (about $11.7M CDBG + about $5.4M state), in design, target end of 2027 | https://hoodline.com/2026/07/fat-city-scores-17-million-park-play-to-jump-start-metairie-nightlife/ |
| midtown-metairie/page | Fat City bounded by Division St, 18th St, Severn Ave, "Metairie Country Club" | REMOVED | Boundary wrong or unverifiable; now "off Veterans Memorial Blvd near Lakeside Shopping Center" | https://www.visitjeffersonparish.com/communities/metairie/ |
| midtown-metairie/page | Clearview City Center $100M | VERIFIED | Kept | https://enr.com/articles/48361-100-million-project-will-repurpose-suburban-mall-as-open-air-city-center |
| midtown-metairie/page | Clearview "residential towers, grocery anchor, structured parking" | CORRECTED | Now 260+ apartments, hotel, about 100,000 sq ft office, restaurants, 14,000 sq ft event green | ENR (above) |
| midtown-metairie/page | "$113M+ Active Investment" | CORRECTED | Now "$117M Planned Investment" ($100M + $17M) | sources above |
| midtown-metairie/page | Metairie 140K+, most populous unincorporated community in Louisiana | VERIFIED | Kept (143,507 in 2020; fifth-largest CDP in U.S.) | https://en.wikipedia.org/wiki/Metairie,_Louisiana |
| nola-east/page | "Winner of the C40 Reinventing Cities Award from the Mayor of New Orleans" | ATTRIBUTION-FIXED | Team People First got an Honorable Mention in C40's 2023 Students Reinventing Cities (Read and Lake Forest Corridors site). The site winner was Imperial College London's ReNew Orleans. Mayor Cantrell honored the team (Sept 25, 2023) and asked for a presentation. Fixed in hero label, hero copy, stat tile and story | https://www.c40reinventingcities.org/en/events/mayor-of-new-orleans-meets-honourable-mention-team-people-first-new-orleans-students-reinventing-cities-1820.html ; https://css.loyno.edu/news/sep-14-2023_loyola-team-wins-honorable-mention-global-students-reinventing-cities-competition ; https://www.c40reinventingcities.org/en/events/new-orleans-winning-team-present-their-project-to-mayor-latoya-cantrell-1828.html |
| nola-east/page | Competition description ("carbon-neutral development for underused sites") | CORRECTED | Now describes Students Reinventing Cities (green, inclusive, climate-resilient neighborhoods) and names the site | https://www.c40.org/news/12-winners-second-students-reinventing-cities-competition-announced/ |
| nola-east/page | Plan components (disaster planning, anaerobic digesters, transit, bike, housing, green jobs, community spaces) | VERIFIED | Matches Loyola/C40 descriptions | Loyola, C40 (above) |
| nola-east/page | "Nearly two decades later" (after Katrina) | CORRECTED | Now "Years later" (it has been 21 years) | n/a |
| ycod/YcodStory, LegislativeTimeline | Co-founded August 2017 in East Aurora | OWNER-CONFIRM | Kept | Question: Can you confirm the founding month (August 2017)? Spectrum says the group met Assemblyman DiPietro in September 2018. |
| ycod/YcodStory, LegislativeTimeline | "I wrote the 2021 revised draft" | OWNER-CONFIRM | Kept, reworded around S4334 | Question: Is the 2021 draft you wrote the text that became Sen. Gallivan's S4334, or a different document? Was there a 2021–22 Assembly version, and if so what is its number? |
| ycod/MediaWall, YcodStory, LegislativeTimeline | CBC, Yahoo News, Business Insider coverage | OWNER-CONFIRM | Removed | Question: Do you have links to the CBC, Yahoo News or Business Insider pieces? Send them and they can go back in. |
| ycod/YcodStory, LegislativeTimeline, MediaWall | Partnerships with WaitList Zero, ONE8FIFTY, Chris Klug Foundation (2018) | OWNER-CONFIRM | Kept | Question: Can you confirm these three partnerships and that they started in 2018? |
| ycod/YcodStory | Managed social in Hootsuite and Trello, designed brand identity; "seven years" | OWNER-CONFIRM | Kept | Question: Confirm these roles and that your involvement ran from 2017 to 2024. |
| ycod/YcodStory | "the coalition is still active" | OWNER-CONFIRM | Kept | Question: Is The YCOD still active in 2026? |
| ycod/YcodStory, LegislativeTimeline | Advocated for the Living Donor Support Act | OWNER-CONFIRM | Kept | Question: What did your advocacy for S1594/A146 involve (testimony, letters, meetings)? |
| ycod/VideoFeature | Subtitle "Organ Donation Advocacy" for the TEDx talk | OWNER-CONFIRM | Kept | Question: Does "The Myth of the Apolitical Youth" cover organ donation enough to keep this subtitle? |
| our-climate/page | Fellowship Nov 2019 to Oct 2020, based in Portland; Dec 2019 campaigns; Feb 2020 D.C. lobby day; "Led virtual town halls" (May 2020) | OWNER-CONFIRM | Kept | Question: Confirm the dates, the D.C. trip, and that you led (not only helped with) the virtual town halls. |
| our-climate/page | "Portland-based fellows organized community pressure" around Oregon EO 20-04 | OWNER-CONFIRM | Kept | Question: Did your cohort work on the Oregon cap-and-trade / EO 20-04 campaign? |
| partnership/page | How Evan joined PPS (was "Future Leaders program"); "Federal Workforce team"; Sept 2021 to Jan 2022 | OWNER-CONFIRM | Future Leaders references removed | Question: What was your program or title at PPS (for example, a PPS fall internship)? Is "Federal Workforce team" the right team name? |
| partnership/page | "Focus Group Facilitation" (Oct 2021), coding framework, summary reports, recommendations | OWNER-CONFIRM | Kept | Question: Did you facilitate focus groups, or transcribe and analyze them? |
| tabi/page | Rural Aurora has fewer wired options than the village; FCC Form 477 vs resident surveys; Chattanooga/Wilson model; 20-year projection; 3–6% property value and 200+ remote-work households | OWNER-CONFIRM | Kept as proposal content; unsourced charts removed | Question: Is TABI written down anywhere (date, PDF)? If the coverage and speed figures came from your own survey or Form 477 analysis, share the source and the charts can come back with a citation. |
| midtown-metairie/page | Proposal sections and recommendations | OWNER-CONFIRM | Kept | Question: Is the proposal still in progress? |
| nola-east/page | Plan figures: 5 MW solar, 500+ rooftop homes, 50 tons/day digester, 12,000 tCO2e/yr, 8.5-mile BRT on Chef Menteur with 12 stations, 15 mi bike lanes, 20 Blue Bikes stations, 3 centers, 30% affordable at 80% AMI, 50% local hire, 1,200+ jobs | OWNER-CONFIRM | Kept, labeled in comments as team estimates | Question: Do these numbers come from People First's submission? The C40 site was the Read and Lake Forest corridors, so was the BRT really on Chef Menteur Hwy? |
| nola-east/page | Evan's role on People First | OWNER-CONFIRM | Copy says "our team" | Question: Loyola lists you among six Loyola students, but the site says Tulane. Which is right? What was your role on the team? |

## Section: studio


| Page/file | Claim (short) | Verdict | Action | Source |
|---|---|---|---|---|
| cinematography/page.tsx | Moten's credits include 12 Years a Slave (2013) | ATTRIBUTION-FIXED | Real credit, but in the locations department. Copy now says he was a location assistant, not a filmmaker credit | https://catalog.afi.com/Catalog/moviedetails/69781 |
| cinematography/page.tsx | Moten's credits include Now You See Me (2013) | VERIFIED | Kept, now framed as "worked on Hollywood productions shot in Louisiana" | https://www.imdb.com/name/nm4328198/ (search snippet: "known for Midnight Special, Now You See Me, 12 Years a Slave") |
| cinematography/page.tsx | Moten "has run Claiborne Avenue Productions for over 20 years" | SOFTENED | Removed "over 20 years". Kept "New Orleans producer and director who runs Claiborne Avenue Productions" (company name: see OWNER-CONFIRM) | Moten directed/produced the Music in NOLA shorts: https://pro.imdb.com/title/tt9257408 |
| cinematography/page.tsx | Camera "BlackMagic Cinema Camera 6K", 6K Super 35, 13 stops, BRAW | CORRECTED | Renamed to Blackmagic Pocket Cinema Camera 6K (equipment list + Plato's Cave caption). The Cinema Camera 6K is full-frame and came out Sept 2023, after the Dec 2020 films. The listed specs match the Pocket 6K (2019) | https://www.cined.com/blackmagic-pocket-cinema-camera-6k-announced-super-35-sensor-and-ef-mount/ ; https://ymcinema.com/2023/09/14/blackmagic-announces-the-full-frame-cinema-camera-6k ; Vimeo upload 2020-12-19 |
| cinematography/page.tsx | Sony a7s II: full frame, S-Log2/S-Log3 | VERIFIED | Source comment added | https://www.bhphotovideo.com/c/product/1255307-REG/sony_alpha_a7s_ii_mirrorless.html |
| cinematography/page.tsx | Cinema Grade: "real-time grading directly on the footage plane" | CORRECTED | Reworded to "color grading plug-in that works directly on the image in the viewer" | https://www.provideocoalition.com/cinema-grade-a-new-way-to-color-grade-footage-inside-of-your-nle/ |
| cinematography/page.tsx | Hero: "Narrative film, documentary, and institutional video" | REMOVED | No documentary work appears on the page. Now "Short film, promotional, and institutional video" | Internal consistency |
| cinematography/page.tsx | The Bridge: "sound design in After Effects" | REMOVED | After Effects isn't an audio tool. Caption now says edited in Premiere Pro | Internal / tool capability |
| cinematography/page.tsx | The Bridge: "about communities separated by infrastructure", verite style | SOFTENED | Replaced with the Vimeo description: written by and starring Henry McLaughlin, shot and edited by Evan. Camera Operator/Editor role verified | https://vimeo.com/491626637 (oEmbed description) |
| cinematography/page.tsx | Plato's Cave: subtitle "Poetic Short Film" while the text says "narrative"; intro "one poetic, one narrative" | CORRECTED | Vimeo title is "Plato's Cave (professional narration)". Subtitle is now "Narrated Short Film" and the intro says "a poetic story and a narrated piece" | https://vimeo.com/api/oembed.json?url=https://vimeo.com/492941431 |
| cinematography/page.tsx | Plato's Cave shown as 2.35:1, "anamorphic lenses" | CORRECTED | The video is delivered 16:9 (426x240), so aspect is now 16:9 and "anamorphic lenses" is removed | Vimeo oEmbed (above) |
| cinematography/page.tsx | Embed 7ya0DAUe5FU labeled "Louisiana Children's Museum – Promotional Ad" | CORRECTED | The YouTube video is actually "Evening Cozy Background Piano Jazz to Relax or Study". Embed relabeled to match. Caption says the museum ad isn't posted publicly | https://www.youtube.com/oembed?url=https://www.youtube.com/watch?v=7ya0DAUe5FU&format=json |
| cinematography/page.tsx | "Client-facing video for museums, universities, and cultural institutions across Louisiana" | SOFTENED | Page documents one museum and one university, both in New Orleans. Copy now names those | Internal consistency |
| cinematography/page.tsx | Louisiana Children's Museum: play-based learning museum in New Orleans | VERIFIED | Kept | https://lcm.org/about/ |
| cinematography/page.tsx | Ambient video: 4K, 60fps, HDR, upstate NY snowy fireplace with jazz | VERIFIED | Matches the YouTube title | YouTube oEmbed for U-o0wAagNbQ |
| cinematography/page.tsx | WWNO is New Orleans' NPR affiliate | VERIFIED | Source comment added. Embed title is "WWNO Sample Show" | https://en.wikipedia.org/wiki/WWNO |
| cinematography/page.tsx | Elevator Review subtitles ("The Series Begins/Sequel/Trilogy Completes") | CORRECTED | Replaced with the actual YouTube titles: The Ideal Elevator, A Step Down, Return to Normalcy | YouTube oEmbed for _6mzmQtPKyQ, TV9xw4Q0eek, 4NqWubbW5g4 |
| cinematography/page.tsx | Showreel / The Bridge are 2.35:1 cinemascope | VERIFIED | oEmbed 426x176 and 426x178 (about 2.4:1) | Vimeo oEmbed |
| glass-art/FiringScheduleChart.tsx + GlassArtContent.tsx | Anneal at 960°F | CORRECTED | Changed to 900°F (482°C), Bullseye's anneal soak. 960°F is a System 96 (COE 96) figure and doesn't fit the Bullseye glass the page names | https://www.bullseyeglass.com/wp-content/uploads/writing-firing-schedules-for-fusing-and-slumping.pdf |
| glass-art (both files) | Full fuse 1480–1500°F | CORRECTED | Changed to 1490°F, 10-minute soak (Bullseye 6mm schedule) | Bullseye PDF (above) |
| glass-art (both files) | Ramp ~300°F/h to 1000°F, no pre-fuse soak | CORRECTED | Changed to 400°F/h to 1225°F with a 45-minute soak, then 600°F/h to the fuse temperature | Bullseye PDF (above) |
| glass-art (both files) | Cool-down "≤50°F/h through strain range", 8–12 h | CORRECTED | Changed to 100°F/h from 900°F to 700°F (2 h), then natural cooling | Bullseye PDF (above) |
| glass-art/GlassArtContent.tsx | "One full cycle takes 16 to 24 hours" | CORRECTED | Bullseye's idealized full-fuse graph for 6mm runs about 12 h. Copy updated, chart x-axis now 0–12.5 h | https://www.bullseyeglass.com/wp-content/uploads/TECHBOOK_ST_idealized_firing_graph.pdf |
| glass-art (both files) | Tack fuse at 1380°F | CORRECTED | Changed to "around 1375°F", where pieces bond but keep their height | https://cdn.shopify.com/s/files/1/1725/1871/files/Glass-Tack-Fusing-Tip-Sheet.pdf |
| glass-art/GlassArtContent.tsx | "Edges start to round at 1300°F" | REMOVED | No source supports 1300°F. The 1400°F rounding in the tip sheet belongs to a different behavior band | Tip sheet (above) |
| glass-art/FiringScheduleChart.tsx | Chart note "Built from the stage table… midpoint of each duration" | CORRECTED | Note now cites Bullseye's published 6mm full-fuse schedule. Code comment marks the AFAP and natural-cool segments as illustrative | Bullseye PDF (above) |
| glass-art/GlassArtContent.tsx | "COE 90 compatible" Bullseye glass | CORRECTED | Bullseye says it doesn't rate its glass "COE 90". Copy now reads "Bullseye Compatible glass (often sold as COE 90)…tests its fusible glasses for compatibility with each other" | https://www.bullseyeglass.com/faq |
| modeling/page.tsx | Meta: "Runway modeling for Vogue Italy's 2020 feature" | SOFTENED | Reworded to "Runway modeling in BizarrAudi's SchoolTime collection, featured by Vogue Italy in 2020" so it doesn't read as modeling for Vogue | Internal consistency with page copy |
| modeling/ModelingContent.tsx | Garment details (blazer, pleated skirt, varsity letter, backpack, silhouettes, recolored fabrics) | REMOVED | No public record of the collection found. Kept the general "recuts school uniform pieces as fashion" | Searches for "BizarrAudi", "SchoolTime" + Vogue Italia found only roden.works |
| modeling/ModelingContent.tsx | Closing line stated as the collection's intent ("SchoolTime uses it to ask…") | SOFTENED | Reframed as Evan's reading ("To me, SchoolTime asks…") | — |
| components/ui/CinemaEmbed.tsx | No factual claims (UI only) | VERIFIED | No change | — |
| OWNER-CONFIRM | Claiborne Avenue Productions | OWNER-CONFIRM | Evan: what is the exact name of Moten's company? No public record of "Claiborne Avenue Productions" turned up (his IMDb credits are Music in NOLA shorts). Was it this name, and was your role camera operator and editor? | — |
| OWNER-CONFIRM | Camera model | OWNER-CONFIRM | Evan: did you shoot on the Blackmagic Pocket Cinema Camera 6K (2019, Super 35)? The site said "Cinema Camera 6K", but that full-frame model came out in 2023. Other pages say "BlackMagic 6K", which is fine either way | — |
| OWNER-CONFIRM | Louisiana Children's Museum ad | OWNER-CONFIRM | Evan: please send the correct link for the LCM promo you directed and edited. The old embed played a background-jazz video. Also confirm your role was Director/Editor | — |
| OWNER-CONFIRM | Plato's Cave | OWNER-CONFIRM | Evan: were you Director of Photography? Who did the "professional narration"? Did you use anamorphic lenses? That claim was removed because the video is 16:9 | — |
| OWNER-CONFIRM | The Bridge | OWNER-CONFIRM | Evan: is "Henry McLaughlin" the right spelling (Vimeo has "Mclaughlin")? Which tool did you use for sound, if you want that mentioned? | — |
| OWNER-CONFIRM | Tulane Freeman School | OWNER-CONFIRM | Evan: confirm you were a videographer for the A.B. Freeman School making promos, faculty interviews, event coverage and social clips, with dates | — |
| OWNER-CONFIRM | WWNO sample show | OWNER-CONFIRM | Evan: was the "WWNO Sample Show" made for WWNO (commissioned, submitted, or aired), or was it a demo in WWNO's format? Is it 1 hour long? | — |
| OWNER-CONFIRM | Ambient video length / elevator durations | OWNER-CONFIRM | Evan: is the upstate NY fireplace video 4 hours long, and is each elevator video about 1:00? YouTube metadata couldn't be read here | — |
| OWNER-CONFIRM | Vogue Italy / BizarrAudi SchoolTime | OWNER-CONFIRM | Evan: can you share a link to Vogue Italy's 2020 coverage? Was it in the magazine, on vogue.it, or a PhotoVogue upload? Who or what is BizarrAudi? Did you appear in an editorial shoot as well as the runway (page says "Runway Show & Editorial")? Should the garment details be restored? | — |
| OWNER-CONFIRM | Glass practice | OWNER-CONFIRM | Evan: do you fire Bullseye glass, and is your own schedule close to Bullseye's 6mm full fuse? Do you use a diamond lap grinder and wet belt sander for cold working? Is the series called "Fractured Futures"? | — |
| OWNER-CONFIRM | Photography | OWNER-CONFIRM | Evan: do you shoot medium format (which camera?) as well as full frame? No publication or exhibition claims were found on the page | — |
