import type { Metadata } from 'next'
import AboutHero from './AboutHero'
import Bio from './Bio'
import Education from './Education'
import Awards from './Awards'
import Languages from './Languages'
import Skills from './Skills'
import ContactSection from './ContactSection'

export const metadata: Metadata = {
  title: 'About',
  description:
    'Evan Roden — Sustainability Engineer, biomedical researcher, nonprofit founder, filmmaker, and TEDx speaker.',
}

export default function AboutPage() {
  return (
    <>
      <AboutHero />
      <Bio />
      <Education />
      <Awards />
      <Languages />
      <Skills />
      <ContactSection />
    </>
  )
}
