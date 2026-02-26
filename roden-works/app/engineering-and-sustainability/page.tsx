import type { Metadata } from 'next'
import PageHero from '@/components/ui/PageHero'
import Timeline from './Timeline'
import CaseStudyGrid from './CaseStudyGrid'
import SkillsRadar from './SkillsRadar'

export const metadata: Metadata = {
  title: 'Engineering & Sustainability',
  description:
    'From biomedical research labs to hospital energy plants — designing systems that keep people alive.',
}

export default function EngineeringPage() {
  return (
    <>
      <PageHero
        title="Engineering & Sustainability"
        subtitle="From biomedical research labs to hospital energy plants — designing systems that keep people alive."
        label="Technical Portfolio"
        variant="dark"
      />
      <Timeline />
      <CaseStudyGrid />
      <SkillsRadar />
    </>
  )
}
