import type { Metadata } from 'next'
import PageHero from '@/components/ui/PageHero'
import AnimatedCard from '@/components/ui/AnimatedCard'
import { ProjectIllustration } from '@/components/ui/ProjectIllustrations'

export const metadata: Metadata = {
  title: 'Advocacy & Civic Impact — Organ Donation, Climate & Urban Policy',
  description:
    'Organ donation law, climate policy, rural broadband, and urban planning.',
}

const projects = [
  {
    href: '/advocacy-and-civic/ycod',
    slug: 'ycod',
    title: 'The Youth Coalition For Organ Donation',
    // "At 17" softened; founders were East Aurora High School students:
    // https://mynews13.com/fl/orlando/news/2021/09/24/wny-teens-nominated-for-american-red-cross-award-for-organ-donation-coalition
    description: 'Co-founded at fifteen. More than seven years of advocacy for presumed consent organ donation legislation in New York.',
    label: 'Founded 2016',
  },
  {
    href: '/advocacy-and-civic/our-climate',
    slug: 'our-climate',
    title: 'Our Climate Fellowship',
    description: 'Youth-led climate policy advocacy at the state and federal level, including campaign organizing and meetings with representatives.',
    label: 'Fellowship',
  },
  {
    href: '/advocacy-and-civic/tabi',
    slug: 'tabi',
    title: 'Aurora Broadband Initiative',
    description: 'Broadband access proposal for underserved rural households in Western New York.',
    label: 'Digital Equity',
  },
  {
    href: '/advocacy-and-civic/nola-east',
    slug: 'nola-east',
    title: 'New Orleans East Revitalization',
    // Honorable mention in C40's Students Reinventing Cities (2023), not the Reinventing Cities award:
    // https://css.loyno.edu/news/sep-14-2023_loyola-team-wins-honorable-mention-global-students-reinventing-cities-competition
    description: 'Revitalization plan covering energy, transit, green housing, and disaster planning. Honorable mention in C40\'s Students Reinventing Cities competition.',
    label: 'C40 Honorable Mention',
  },
  {
    href: '/advocacy-and-civic/midtown-metairie',
    slug: 'midtown-metairie',
    title: 'Midtown Metairie',
    // Metairie CDP, pop. 143,507 (2020), the largest CDP in Louisiana:
    // https://en.wikipedia.org/wiki/List_of_census-designated_places_in_Louisiana
    description: 'Urban planning proposal for Louisiana\'s most populous unincorporated community.',
    label: 'Urban Planning',
  },
  {
    href: '/advocacy-and-civic/partnership',
    slug: 'partnership',
    title: 'Partnership for Public Service',
    // SAMHSA's Best Places to Work score went from roughly 37 to 74 within three years of partnering (Aug 2021):
    // https://ourpublicservice.org/about/history-and-impact/samhsa-strong-teaming-up-to-transform-the-workplace
    description: 'Federal workforce project with SAMHSA. The agency\'s employee engagement score rose from about 37 to 74 over the Partnership\'s broader collaboration.',
    label: 'Federal Service',
  },
]

export default function AdvocacyPage() {
  return (
    <>
      <PageHero
        title="Advocacy & Civic Impact"
        subtitle="Organ donation law, climate policy, rural broadband, and urban planning."
        label="Public Service"
        variant="forest"
      />
      <section className="section-padding bg-slate-950">
        <div className="content-width">
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
            {projects.map((project, i) => (
              <AnimatedCard key={project.href} index={i} {...project}>
                <div className="mb-4 -mx-2">
                  <ProjectIllustration slug={project.slug} variant="card" />
                </div>
              </AnimatedCard>
            ))}
          </div>
        </div>
      </section>
    </>
  )
}
