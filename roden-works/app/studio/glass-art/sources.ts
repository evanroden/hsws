import type { Source } from '@/components/ui/Sources'

// Order = order of first citation on the page, so footnote numbers read 1, 2, 3… top to bottom.
export const GLASS_ART_SOURCES: Source[] = [
  {
    id: 'bullseye-faq',
    title: 'Frequently Asked Questions',
    publisher: 'Bullseye Glass Co.',
    url: 'https://www.bullseyeglass.com/faq',
    note: 'Bullseye does not rate its glass “COE 90”; it tests its fusible glasses for compatibility with each other.',
  },
  {
    id: 'bullseye-graph',
    title: 'Studio Tips: Idealized Firing Graph',
    publisher: 'Bullseye Glass Co.',
    url: 'https://www.bullseyeglass.com/wp-content/uploads/TECHBOOK_ST_idealized_firing_graph.pdf',
    note: 'Full-fuse cycle for two 3mm layers spanning about 12 hours.',
  },
  {
    id: 'bullseye-schedule',
    title: 'Studio Tips: Writing Firing Schedules for Fusing & Slumping',
    publisher: 'Bullseye Glass Co.',
    url: 'https://www.bullseyeglass.com/wp-content/uploads/writing-firing-schedules-for-fusing-and-slumping.pdf',
    note: '6mm full-fuse example: 400°F/h to 1225°F, hold 0:45; 600°F/h to 1490°F, hold 0:10; AFAP to 900°F, hold 1:00; 100°F/h to 700°F.',
  },
  {
    id: 'glacial-tack',
    title: 'Tack Fuse Tip Sheet',
    publisher: 'Glacial Art Glass',
    url: 'https://cdn.shopify.com/s/files/1/1725/1871/files/Glass-Tack-Fusing-Tip-Sheet.pdf',
    note: 'At 1375°F pieces are fused firmly while keeping their height and shape.',
  },
]
