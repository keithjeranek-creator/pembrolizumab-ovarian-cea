# Shared reference table and marker map, imported by both assemblers.
#
# Access dates are LITERALS, not {TODAY_LONG}. An access date records when a
# person consulted the source; stamping it with the build date silently rewrote
# five references from "September 1, 2026" to whatever day the script last ran.
# Change one of these only when the source is actually re-checked.
# Single source of truth: duplicating this list is how one copy goes stale.
REFS = [
 ('P','41528114'), ('P','41974150'), ('P','24637997'), ('P','31046082'), ('P','34143970'),
 ('T', 'US Food and Drug Administration. Drugs@FDA: FDA-approved drugs. Pembrolizumab, BLA 125514, '
       'supplement 186; approved February 10, 2026. Accessed September 1, 2026. '
       'https://www.accessdata.fda.gov/scripts/cder/daf/index.cfm?event=overview.process&ApplNo=125514'),
 ('P','35098747'),
 ('T', 'US Bureau of Labor Statistics. Consumer Price Index for All Urban Consumers: medical care '
       'services in US city average. Series CUUR0000SAM2. Accessed September 1, 2026. https://www.bls.gov/cpi/'),
 ('P','33248517'), ('P','27623463'), ('P','23341049'), ('P','31707911'), ('P','19217148'), ('P','29464667'),
 ('T', 'Red Book Online. Merative US L.P.; 2026. Accessed August 3, 2026. https://www.micromedexsolutions.com'),
 ('T', 'Centers for Medicare & Medicaid Services. Medicare Part B drug payment limit file: January 2026. '
       'Accessed September 1, 2026. https://www.cms.gov/medicare/payment/part-b-drugs/asp-pricing-files'),
 ('T', 'Centers for Medicare & Medicaid Services. PFS relative value files: RVU26A. '
       'Accessed September 1, 2026. '
       'https://www.cms.gov/medicare/payment/fee-schedules/physician/pfs-relative-value-files'),
 ('P','38777864'), ('P','29652926'), ('P','35518812'), ('P','25162885'),
 ('T', 'Social Security Act §1182(e), 42 USC §1320e-1(e). '
       'Accessed September 1, 2026. https://www.ssa.gov/OP_Home/ssact/title11/1182.htm'),
 ('T', 'Inflation Reduction Act of 2022, Pub L No. 117-169, 136 Stat 1818 (2022).'),
]
MAP = {
 'Siegel 2026, PMID 41528114':1,'Colombo 2026, PMID 41974150':2,'Pujade-Lauraine 2014, PMID 24637997':3,
 'Matulonis 2019, PMID 31046082':4,'Pujade-Lauraine 2021, PMID 34143970':5,
 'FDA, BLA 125514 supplement 186':6,'FDA prescribing information, BLA 125514 s186, approved 10 Feb 2026':6,
 'Husereau 2022, PMID 35098747':7,'BLS CPI series CUUR0000SAM2':8,'Woods 2020, PMID 33248517':9,
 'Sanders 2016, PMID 27623463':10,'Latimer 2013, PMID 23341049':11,'Bell Gorrod 2019, PMID 31707911':12,
 'Havrilesky 2009, PMID 19217148':13,'Ball 2018, PMID 29464667':14,'Red Book Online, Merative':15,
 'CMS Part B Payment Limit File, January 2026':16,'CMS PFS Relative Value File RVU26A':17,
 'Flanigan 2024, PMID 38777864':18,'Wong 2018, PMID 29652926':19,'Shaka 2022, PMID 35518812':20,
 'Neumann 2014, PMID 25162885':21,'Social Security Act §1182(e)':22,
 'Inflation Reduction Act of 2022, Pub L No. 117-169':23,
}
