import { TestBed, async, inject } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { HttpModule } from '@angular/http';
import { InfoMessagesService } from '../info-messages.service';

import { TaskListComponent } from './task-list.component';

describe('TaskListComponent', () => {
    let fixture;
    let nodes;

    beforeEach(async(() => {
        TestBed.configureTestingModule({
            declarations: [
                TaskListComponent
            ],
            imports: [
                RouterTestingModule,
                HttpModule
            ]
        }).compileComponents();
        fixture = TestBed.createComponent(TaskListComponent);
        nodes = fixture.debugElement.nativeElement;
    }));

    it('should not have an <hr> element',  async(() => {
        expect(nodes.querySelectorAll('hr').length).toEqual(0);
    }));

    it('should have a delete button', async(() => {
        fixture.componentInstance.tasks = [{name: 'test remove', happy: true}];
        fixture.detectChanges();

        expect(nodes.querySelectorAll('button > .glyphicon-remove').length)
            .toBeGreaterThan(0);
    }));
});
