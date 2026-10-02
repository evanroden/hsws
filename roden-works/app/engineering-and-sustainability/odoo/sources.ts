import type { Source } from '@/components/ui/Sources'

// Order = order of first citation on the page, so footnote numbers read 1, 2, 3… top to bottom.
export const ODOO_SOURCES: Source[] = [
  {
    id: 'odoo-about',
    title: 'About Us',
    publisher: 'Odoo',
    url: 'https://www.odoo.com/page/about-us',
    note: '28 million users; 50 main applications; 50,000+ community apps; founder Fabien Pinckaers.',
  },
  {
    id: 'summit-2024',
    title: "Odoo S.A. announces a €500 million transaction, increasing the Belgian Unicorn's valuation to €5 billion",
    publisher: 'Summit Partners',
    date: 'Nov 20, 2024',
    url: 'https://www.summitpartners.com/news/odoo-announces-a-500-million-transaction-increasing-the-belgian-unicorns-valuation-to-5-billion',
    note: '€500M secondary led by CapitalG and Sequoia Capital, BlackRock among participants, €5B valuation; Belgian company founded by Fabien Pinckaers.',
  },
  {
    id: 'brussels-times-2026',
    title: 'Odoo announces €10-billion valuation and a partial price increase',
    publisher: 'The Brussels Times',
    date: 'Sept 24, 2026',
    url: 'https://www.brusselstimes.com/2332306/odoo-announces-e10-billion-valuation-and-a-partial-price-increase',
    note: '€10 billion valuation.',
  },
  {
    id: 'odoo-editions',
    title: 'Odoo Enterprise vs Community | Odoo Editions Comparison',
    publisher: 'Odoo',
    url: 'https://www.odoo.com/page/editions',
    note: 'Community (open-source) and Enterprise (licensed) editions; analytic accounting, barcode and IoT marked Enterprise-only.',
  },
]
