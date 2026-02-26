export const SITE_CONFIG = {
  name: 'Evan Joseph Roden',
  title: 'Evan Joseph Roden — Engineering, Advocacy, Creative',
  description:
    'Optimizing complex systems to improve human quality of life at the intersection of engineering, sustainability, and public advocacy.',
  url: 'https://roden.works',
  email: 'evanjroden@gmail.com',
  phone: '(716) 418-4157',
  linkedin: 'https://linkedin.com/in/evanroden',
  instagram: 'https://instagram.com/evanroden1',
  tiktok: 'https://tiktok.com/@evanroden1',
} as const

export const NAV_ITEMS = [
  { label: 'Home', href: '/' },
  { label: 'Engineering', href: '/engineering-and-sustainability' },
  { label: 'Advocacy', href: '/advocacy-and-civic' },
  { label: 'Studio', href: '/studio' },
  { label: 'About', href: '/about' },
] as const

export const IMPACT_STATS = [
  { value: 160, suffix: '% NRR', label: 'Net Revenue Retention at Odoo', prefix: '' },
  { value: 143.8, suffix: 'M', label: 'EaaS Partnership Value', prefix: '$' },
  { value: 7, suffix: '+ Years', label: 'Leading The YCOD', prefix: '' },
  { value: 3, suffix: '', label: 'Research Labs at Tulane', prefix: '' },
  { value: 354.6, suffix: 'M', label: '30-Year Guaranteed Savings', prefix: '$' },
] as const

export const COLORS = {
  slate950: '#0B1215',
  titanium: '#8A9BA8',
  forest: '#1B3A2D',
  forestLight: '#2D5A45',
  cream: '#F5F2ED',
  white: '#FAFAFA',
  copper: '#B87333',
  clinical: '#E8EDF1',
} as const
