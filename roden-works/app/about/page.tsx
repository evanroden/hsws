import type { Metadata } from 'next'
import fs from 'node:fs'
import path from 'node:path'
import AboutHero from './AboutHero'
import Bio from './Bio'
import Education from './Education'
import Awards from './Awards'
import Languages from './Languages'
import Skills from './Skills'
import ContactSection from './ContactSection'
import { PersonJsonLd } from '@/components/seo/JsonLd'

export const metadata: Metadata = {
  title: 'About',
  description:
    'Evan Roden is a sustainability engineer at ENFRA, a Tulane biomedical engineering graduate, co-founder of the Youth Coalition For Organ Donation, a filmmaker, and a TEDx speaker.',
}

export default function AboutPage() {
  // Offer a download only when a resume has actually been added to /public
  const hasResume = fs.existsSync(path.join(process.cwd(), 'public', 'resume.pdf'))

  return (
    <>
      <PersonJsonLd />
      <AboutHero hasResume={hasResume} />
      <Bio />
      <Education />
      <Awards />
      <Languages />
      <Skills />
      <ContactSection />
    </>
  )
}
